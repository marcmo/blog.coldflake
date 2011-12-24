/*
 * VoltageHysteresis.h
 *
 *  Created on: 15/03/2010
 *      Author: oruizdorantes
 */

#ifndef VOLTAGEHYSTERESIS_H_
#define VOLTAGEHYSTERESIS_H_

#include "HysteresisEntity.h"

typedef enum {
	UNDER_VOLTAGE,
	NORMAL_VOLTAGE,
	OVER_VOLTAGE
} tVoltageState;

typedef enum {
	UNDER_VOLTAGE_LEVEL = 6000,
	OVER_VOLTAGE_LEVEL = 15500,
	NORMAL_VOLTAGE_LEVEL = 12000,
	HYSTERESIS_VOLTAGE_STEP = 500
} tVoltageLevel;

typedef HysteresisEntity<tVoltageState, 15500, 16000> OverVoltageHysteresis2;
typedef HysteresisEntity<tVoltageState, 6000, 6500> UnderVoltageHysteresis2;


#endif /* VOLTAGEHYSTERESIS_H_ */
