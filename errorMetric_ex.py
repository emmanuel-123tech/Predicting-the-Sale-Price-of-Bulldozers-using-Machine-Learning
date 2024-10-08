# -*- coding: utf-8 -*-

import csv
import sys

reference_path = sys.argv[1]
submission_path = sys.argv[2]

strategyPath = submission_path


def read_referenceData(reference_path):
    loadDict = {}
    solarDict = {}

    siteConfigPath = reference_path
    siteConfig = {}
    count = 0
    try:
        with open(siteConfigPath) as file:
            csv_reader = csv.reader(file)
            header = next(csv_reader)
            for row in csv_reader:
                count += 1
                if count < 1681:
                    key1, key2, value, value1 = row[0], row[1], row[2], row[3]
                    if key1 not in loadDict:
                        loadDict[key1] = {}
                        solarDict[key1] = {}
                    loadDict[key1][key2] = float(value) * 1000
                    solarDict[key1][key2] = float(value1) * 1000
                    continue
                siteName = row[0]
                dieselPower = float(row[1]) * 1000
                initSoc = float(row[6])
                ratedCapacity = float(row[2])
                ratedVoltage = float(row[3])
                dod = float(row[7])
                coe = float(row[4])
                disCoe = float(row[5])
                gridPower = float(row[8]) * 1000
                gridPlan = row[9][1:len(row[9]) - 1].split(" ")
                plan = []
                for i in range(0, len(gridPlan)):
                    plan.append(gridPlan[i].lower() == "true")

                config = {"site": siteName, "dieselPower": dieselPower, "initSoc": initSoc,
                          "ratedCapacity": ratedCapacity, "ratedVoltage": ratedVoltage, "DOD": dod, "coe": coe,
                          "disCoe": disCoe,
                          "gridPower": gridPower, "gridPlan": plan, "solarPower": solarDict[siteName],
                          "loadPower": loadDict[siteName]}
                siteConfig[siteName] = config
        return siteConfig
    except UnicodeDecodeError:
        print("error: unknown file type")
        quit()


def read_strategyData(strategyPath):
    strategyDict = {}
    try:
        with open(strategyPath) as file:
            csv_reader = csv.reader(file)
            header = next(csv_reader)
            for row in csv_reader:
                siteName = row[0]
                index = row[1]
                grid = row[2].lower() == "true"
                diesel = row[3].lower() == "true"
                solar = row[4].lower() == "true"
                batteryStatues = {"useGrid": grid, "useDiesel": diesel, "useSolar": solar}
                if siteName not in strategyDict:
                    strategyDict[siteName] = {}
                strategyDict[siteName][index] = batteryStatues

        return strategyDict
    except UnicodeDecodeError:
        print("error: unknown file type")
        quit()


def getDieselNum(strategy, predictLength):
    dieselNum = 0.0
    if strategy['0']["useDiesel"]:
        dieselNum += 1
    for i in range(0, predictLength * 4 - 1):
        batteryStatus = strategy[str(i)]
        nextBatteryStatus = strategy[str(i + 1)]
        if (not batteryStatus["useDiesel"]) and nextBatteryStatus["useDiesel"]:
            dieselNum += 1

    return dieselNum


def getTotalDieselTime(strategy, predictLength):
    totalTime = 0.0
    for i in range(0, predictLength * 4):
        batteryStatus = strategy[str(i)]
        if batteryStatus["useDiesel"]:
            totalTime += 60.0 / 4

    return totalTime


def getMaxTime(strategy, predictLength):
    maxTime = 0.0
    for i in range(0, predictLength * 4):
        batteryStatus = strategy[str(i)]
        if not batteryStatus["useDiesel"]:
            continue
        curTime = 0.0
        while strategy[str(i)]["useDiesel"]:
            curTime += 60.0 / 4
            i += 1
            if i == predictLength * 4:
                break

        if curTime > maxTime:
            maxTime = curTime

    return maxTime


def getMae(config, predictSolarPower, predictLength):
    mae = 0.0
    sum = 0.0
    for i in range(0, predictLength):
        sum += abs(config["solarPower"][str(i)])
        mae += abs(predictSolarPower[str(i)] - config["solarPower"][str(i)])

    mae /= sum
    return mae


def getTotalGridTime(strategy, predictLength):
    totalTime = 0.0
    for i in range(0, predictLength * 4):
        batteryStatus = strategy[str(i)]
        if batteryStatus["useGrid"]:
            totalTime += 60.0 / 4

    return totalTime


def calcSingleSiteScore(strategy, config, predictLength):
    curSoc = float(config["initSoc"])
    capacity = float(config["ratedCapacity"]) * float(config["ratedVoltage"])
    coe = float(config["coe"])
    for i in range(0, predictLength * 4):
        batteryStatus = strategy[str(i)]
        diePower = config["dieselPower"] if batteryStatus["useDiesel"] else 0.0
        solarPower = config["solarPower"][str(i // 4)] if batteryStatus["useSolar"] else 0.0
        gridPower = config["gridPower"] if batteryStatus["useGrid"] and config["gridPlan"][i // 4] else 0.0
        loadPower = config["loadPower"][str(i // 4)]

        sum = diePower + solarPower + gridPower - loadPower
        if sum < 0:
            coe = float(config["disCoe"])
        curSoc += sum * coe / (4 * capacity)
        curSoc = min(curSoc, 1.0)
        if curSoc < config["DOD"]:
            return -1

    return 300 * getDieselNum(strategy, predictLength) + getTotalDieselTime(strategy,
                                                                            predictLength) + 0.95 * getMaxTime(strategy,
                                                                                                               predictLength) + 0.25 * getTotalGridTime(
        strategy, predictLength)


def scoreCalculator(strategyPath, reference_path, days):
    totalTime = 7 * 24 * 4
    predictLength = 24 * days
    siteConfig = read_referenceData(reference_path)
    siteStrategy = read_strategyData(strategyPath)
    if not siteStrategy:
        return -1.0
    siteNum = 10
    if len(siteStrategy.keys()) != 10:
        print("The number of sites is wrong, please check your submission")
        return -1.0
    result = 0.0
    for siteName in siteStrategy.keys():
        if siteName not in siteConfig or len(siteStrategy[siteName]) != totalTime:
            print("The site name or time length is wrong, please check your submission")
            return -1.0
        siteScore = calcSingleSiteScore(siteStrategy[siteName], siteConfig[siteName],
                                        predictLength)
        if siteScore < 0:
            print("The strategy is unfeasible, please re-design your strategy")
            return -1.0
        result += siteScore
    return result / siteNum


print("The score is ", scoreCalculator(strategyPath, reference_path, days))