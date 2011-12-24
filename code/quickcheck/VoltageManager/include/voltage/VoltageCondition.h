/*
 * VoltageCondition.h
 *
 *  Created on: 15/03/2010
 *      Author: oruizdorantes
 */

#ifndef VOLTAGECONDITION_H_
#define VOLTAGECONDITION_H_

#include "voltage/VoltageHysteresis.h"

namespace bsp { class IVoltageListener; }

namespace bios
{
class VoltageCondition
{

public:
	VoltageCondition(bsp::IVoltageListener&);

	void cyclic();

private:

	static const uint16 GAP = 100;

	bsp::IVoltageListener& 	fVoltageListener;

	uint32					fReferenceVoltageL1;
	uint32					fReferenceVoltageL2;

	bool					fInitialized;

};
} //namespace bios


#endif /* VOLTAGECONDITION_H_ */
