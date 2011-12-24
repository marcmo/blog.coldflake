/*
 * VoltageCondition.cpp
 *
 *  Created on: 15/03/2010
 *      Author: oruizdorantes
 */

#include "voltage/VoltageCondition.h"
#include "bsp/power/IVoltageListener.h"
#include "adc/AnalogInput.h"
#include "adc/ADScaling.h"

namespace bios
{
VoltageCondition::VoltageCondition(bsp::IVoltageListener& listener)
	: fVoltageListener(listener)
	, fReferenceVoltageL1(NORMAL_VOLTAGE_LEVEL)
	, fReferenceVoltageL2(NORMAL_VOLTAGE_LEVEL)
	, fInitialized(false)
{

}

// for testing purposes, move inFunc
void VoltageCondition::cyclic()
{
	uint16 voltageL1 =0;
	uint16 voltageL2 =0;
	ADScaling::get(AnalogInput::AiTerm30L1, voltageL1);
	ADScaling::get(AnalogInput::AiTerm30L2, voltageL2);

	if (!fInitialized) {
		fReferenceVoltageL1 = voltageL1;
		fReferenceVoltageL2 = voltageL2;
		fInitialized = true;
	}

	if ((voltageL1 > fReferenceVoltageL1 + GAP) || (voltageL1 < fReferenceVoltageL1 - GAP)) {
		fReferenceVoltageL1 = voltageL1;
		fVoltageListener.voltageChanged(voltageL1, bsp::IVoltageListener::BATTERY_L1);
	}
	if ((voltageL2 > fReferenceVoltageL2 + GAP) || (voltageL2 < fReferenceVoltageL2 - GAP)) {
		fReferenceVoltageL2 = voltageL2;
		fVoltageListener.voltageChanged(voltageL2, bsp::IVoltageListener::BATTERY_L2);
	}
}


}//namespace bios
