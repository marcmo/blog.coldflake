/**
 * Contains interface of a class which provides Informations about the voltage on the board.
 * @file 		IVoltageManager.h
 * @ingroup		bsp
 *
 * (c) BMW AG
 */
#ifndef IVOLTAGEMANAGER_H_
#define IVOLTAGEMANAGER_H_

#include "commonTypes.h"

namespace bsp
{
class AbstractPowerStateListener;
class ICriticalVoltageListener;

/**
 * @class		IVoltageManager
 *
* Interface of a class which provides Informations about the voltage on the board.
 */
class IVoltageManager
{
public:
	/**
	 * Destructor
	 */
	virtual ~IVoltageManager() {}

	/**
	 * registers a PeakUnderVoltageListener
	 * listener will be called everytime we enter or leave undervoltage
	 * @param listener The listener to register
	 **/
	virtual void registerUnderVoltageListener(ICriticalVoltageListener&) = 0;
	virtual void removeUnderVoltageListener(ICriticalVoltageListener&) = 0;
	virtual void registerOverVoltageListener(ICriticalVoltageListener&) = 0;
	virtual void removeOverVoltageListener(ICriticalVoltageListener&) = 0;
    virtual void removeAllUnderVoltageListeners() = 0;
    virtual void removeAllOverVoltageListeners() = 0;

	/**
	 * Returns the current voltage
	 * @return The current voltage millivolts
	 **/
	virtual uint16 getCurrentVoltage() = 0;

};

}

#endif /*IVOLTAGEMANAGER_H_*/
