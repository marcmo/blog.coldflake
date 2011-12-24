/*
 * AbstractCriticalVoltageListener.h
 *
 *  Created on: Apr 26, 2010
 *      Author: omueller
 */

#ifndef ABSTRACTCRITICALVOLTAGELISTENER_H_
#define ABSTRACTCRITICALVOLTAGELISTENER_H_

#include "bsp/power/ICriticalVoltageListener.h"
#include "bsp/power/IVoltageListener.h"
#include "util/timeout/AbstractTimeout.h"
#include "util/timeout/ITimeoutManager2.h"
#include "stdio.h"


namespace bios
{
class UnderVoltageStrategy
{
public:
	static bool moreCritical(uint16 newV, uint16 oldV)
	{
		return (newV < oldV);
	}
	static bool lessCritical(uint16 newV, uint16 oldV)
	{
		return (newV > oldV);
	}
	static uint16 hysteresisLevel(uint16 notificationVoltage, uint16 hysteresis)
	{
		return notificationVoltage + hysteresis;
	}
};
class OverVoltageStrategy
{
public:
	static bool moreCritical(uint16 newV, uint16 oldV)
	{
		return (newV > oldV);
	}
	static bool lessCritical(uint16 newV, uint16 oldV)
	{
		return (newV < oldV);
	}
	static uint16 hysteresisLevel(uint16 notificationVoltage, uint16 hysteresis)
	{
		return notificationVoltage - hysteresis;
	}
};
template<typename T>
class AbstractCriticalVoltageListener :
	public bsp::ICriticalVoltageListener,
	public common::AbstractTimeout
{
public:
	AbstractCriticalVoltageListener(
			common::ITimeoutManager2& timeoutManager,
			uint16 notificationVoltage,
			bsp::IVoltageListener::Battery b,
			uint32 debounce) :
		fTimeoutManager(timeoutManager),
		fNotificationVoltage(notificationVoltage),
	 	fBattery(b),
	  	fDebounceTime(debounce),
		fHysteresis(DEFAULT_HYSTERESIS),
	  	fVoltageState(NORMAL_VOLTAGE)
	{}

	virtual uint16 getNotificationVoltage()
	{
		return fNotificationVoltage;
	}
	virtual bsp::IVoltageListener::Battery batteryToObserve()
	{
		return fBattery;
	}
	virtual uint16 getHysteresis()
	{
		return fHysteresis;
	}

	virtual void voltageLessCritical(uint16 newVoltage)
	{
		switch (fVoltageState)
		{
			case ABNORMAL_VOLTAGE_PENDING:
				if (T::lessCritical(newVoltage, fNotificationVoltage))
				{
					fTimeoutManager.cancel(*this);
					fVoltageState = NORMAL_VOLTAGE;
				}
				break;
			case ABNORMAL_VOLTAGE:
				if (T::lessCritical(newVoltage, T::hysteresisLevel(fNotificationVoltage, fHysteresis)))
				{
					fVoltageState = NORMAL_VOLTAGE;
					criticalVoltageLeft();
				}
				else if (T::lessCritical(newVoltage, fNotificationVoltage))
				{
					fVoltageState = HYSTERESIS_VOLTAGE;
				}
				break;
			case HYSTERESIS_VOLTAGE:
				if (T::lessCritical(newVoltage, T::hysteresisLevel(fNotificationVoltage, fHysteresis)))
				{
					fVoltageState = NORMAL_VOLTAGE;
					criticalVoltageLeft();
				}
				break;
			default:
				break;
		}

	}
	virtual void voltageMoreCritical(uint16 newVoltage)
	{
		if (T::moreCritical(newVoltage, fNotificationVoltage))
		{
			switch (fVoltageState)
			{
				case NORMAL_VOLTAGE:
					if(fDebounceTime)
					{
						fVoltageState = ABNORMAL_VOLTAGE_PENDING;
						fTimeoutManager.cancel(*this);
						fTimeoutManager.set(*this, fDebounceTime, false);
					}
					else
					{
						fVoltageState = ABNORMAL_VOLTAGE;
						criticalVoltageEntered();
					}
					break;
				case HYSTERESIS_VOLTAGE:
					fVoltageState = ABNORMAL_VOLTAGE;
					break;
				default:
					break;
			}
		}
	}

	virtual void reset()
	{
		fVoltageState = NORMAL_VOLTAGE;
	}

    /** @implements AbstractTimeout */
	virtual void expired(TimeoutExpiredActions actions)
	{
		if (ABNORMAL_VOLTAGE_PENDING == fVoltageState)
		{
			fVoltageState = ABNORMAL_VOLTAGE;
			criticalVoltageEntered();
		}
	}
	void initialize(
			uint16 voltageLevel,
			uint32 debounceTime,
			bsp::IVoltageListener::Battery battery,
			uint16 hysteresis)
	{
		fNotificationVoltage = voltageLevel;
		fDebounceTime = debounceTime;
		fBattery = battery;
		fHysteresis = hysteresis;
	}
	void setVoltageLevel(uint16 voltageLevel)
	{
		fNotificationVoltage = voltageLevel;
	}
	void setDebounceTime(uint32 debounceTime)
	{
		fDebounceTime = debounceTime;
	}
	void setBattery(bsp::IVoltageListener::Battery battery)
	{
		fBattery = battery;
	}
	void setHysteresis(uint16 h)
	{
		fHysteresis = h;
	}
	enum VoltageState
	{
		NORMAL_VOLTAGE,
		ABNORMAL_VOLTAGE_PENDING,
		ABNORMAL_VOLTAGE,
		HYSTERESIS_VOLTAGE
	};
	VoltageState getVoltageState() const
	{
		return fVoltageState;
	}

private:
	enum { DEFAULT_HYSTERESIS = 500 };
	common::ITimeoutManager2&		fTimeoutManager;
	uint16 							fNotificationVoltage;
	bsp::IVoltageListener::Battery 	fBattery;
	uint32 							fDebounceTime;
	uint16							fHysteresis;
	VoltageState					fVoltageState;
};

typedef AbstractCriticalVoltageListener<UnderVoltageStrategy> AbstractUnderVoltageListener;
typedef AbstractCriticalVoltageListener<OverVoltageStrategy> AbstractOverVoltageListener;

}

#endif /* ABSTRACTCRITICALVOLTAGELISTENER_H_ */
