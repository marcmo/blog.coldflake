/*
 * IVoltageListener.h
 *
 *  Created on: Mar 29, 2010
 *      Author: omueller
 */

#ifndef IVOLTAGELISTENER_H_
#define IVOLTAGELISTENER_H_

#include "commonTypes.h"

namespace bsp
{

class IVoltageListener
{
public:
	enum Battery
	{
		BATTERY_L1,
		BATTERY_L2
	};
	virtual void voltageChanged(uint16 newVoltage, Battery battery) = 0;
};

}

#endif /* IVOLTAGELISTENER_H_ */
