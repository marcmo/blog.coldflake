#include "voltage/VoltageManager.h"
#ifndef UNIT_TEST
#include "adc/AnalogInput.h"
#include "adc/ADScaling.h"
#endif

using namespace bsp;
using namespace bios;
using namespace common;

#include "commonDebug.h"
namespace bios
{
VoltageManager::VoltageManager()
	:	fLastReportedVoltageL1(NORMAL_VOLTAGE),
	 	fLastReportedVoltageL2(NORMAL_VOLTAGE),
	 	fL1_l2_initialized(false)
{}

void VoltageManager::voltageChanged(uint16 voltage, Battery battery)
{
    switch (battery)
    {
    	case IVoltageListener::BATTERY_L1:
		    setNotifyFlagForListeners(fLastReportedVoltageL1, voltage, battery);
		    fLastReportedVoltageL1 = voltage;
		    break;
    	case IVoltageListener::BATTERY_L2:
		    setNotifyFlagForListeners(fLastReportedVoltageL2, voltage, battery);
		    fLastReportedVoltageL2 = voltage;
		    break;
    	default:
    		;
    }
}

void VoltageManager::setNotifyFlagForListeners(uint16 oldVoltage, uint16 newVoltage, Battery battery)
{
    if (newVoltage < oldVoltage)
    {
        for (tUnderVoltageListeners::iterator iter = fUnderVoltageListeners.begin(); iter != fUnderVoltageListeners.end(); ++iter)
        {
            if (iter->batteryToObserve() == battery)
            {
				iter->voltageMoreCritical(newVoltage);
            }
        }
        for (tOverVoltageListeners::iterator iter = fOverVoltageListeners.begin(); iter != fOverVoltageListeners.end(); ++iter)
        {
            if (iter->batteryToObserve() == battery)
            {
				iter->voltageLessCritical(newVoltage);
            }
        }
    }
    //FIXME: optimization: only check listeners if any listener is interested in this step
    if (newVoltage > oldVoltage)
    {
        for (tOverVoltageListeners::iterator iter = fOverVoltageListeners.begin(); iter != fOverVoltageListeners.end(); ++iter)
        {
            uint16 notifyLevel = iter->getNotificationVoltage();
            if ((iter->batteryToObserve() == battery) && (newVoltage > notifyLevel) && (oldVoltage <= notifyLevel))
            {
                iter->voltageMoreCritical(newVoltage);
            }
        }
        for (tUnderVoltageListeners::iterator iter = fUnderVoltageListeners.begin(); iter != fUnderVoltageListeners.end(); ++iter)
        {
            if (iter->batteryToObserve() == battery)
            {
				iter->voltageLessCritical(newVoltage);
            }
        }
    }
}

void VoltageManager::registerUnderVoltageListener(ICriticalVoltageListener& listener)
{
    fUnderVoltageListeners.push_back(listener);
    if (!fL1_l2_initialized)
    {
	    fLastReportedVoltageL1 = getCurrentVoltage();
	    fLastReportedVoltageL2 = getCurrentVoltageL2();
	    fL1_l2_initialized = true;
    }
    if (BATTERY_L1 == listener.batteryToObserve())
    {
	    if (fLastReportedVoltageL1 < listener.getNotificationVoltage())
	    {
	    	listener.voltageMoreCritical(fLastReportedVoltageL1);
	    }
    }
    else if (BATTERY_L2 == listener.batteryToObserve())
	{
	    if (fLastReportedVoltageL2 < listener.getNotificationVoltage())
	    {
	    	listener.voltageMoreCritical(fLastReportedVoltageL2);
	    }
    }
}

void VoltageManager::removeUnderVoltageListener(ICriticalVoltageListener& listener)
{
    if (fUnderVoltageListeners.contains(listener))
    {
        fUnderVoltageListeners.remove(listener);
    	listener.reset();
    }
}

void VoltageManager::registerOverVoltageListener(ICriticalVoltageListener& listener)
{
    fOverVoltageListeners.push_back(listener);
    if (!fL1_l2_initialized)
    {
	    fLastReportedVoltageL1 = getCurrentVoltage();
	    fLastReportedVoltageL2 = getCurrentVoltageL2();
	    fL1_l2_initialized = true;
    }
    if (BATTERY_L1 == listener.batteryToObserve())
    {
	    if (fLastReportedVoltageL1 > listener.getNotificationVoltage())
	    {
	    	listener.voltageMoreCritical(fLastReportedVoltageL1);
	    }
    }
    else if (BATTERY_L2 == listener.batteryToObserve())
	{
	    if (fLastReportedVoltageL2 > listener.getNotificationVoltage())
	    {
	    	listener.voltageMoreCritical(fLastReportedVoltageL2);
	    }
    }
}

void VoltageManager::removeOverVoltageListener(ICriticalVoltageListener& listener)
{
    if (fOverVoltageListeners.contains(listener))
    {
        fOverVoltageListeners.remove(listener);
    	listener.reset();
    }
}
uint16 VoltageManager::getCurrentVoltage()
{
#ifndef UNIT_TEST
	uint16 spannung = 0;
	bios::ADScaling::get( bios::AnalogInput::AiTerm30L1, spannung);
	return (spannung);
#else
    return fLastReportedVoltageL1;
#endif

}

void VoltageManager::removeAllUnderVoltageListeners()
{
	fUnderVoltageListeners.clear();
}
void VoltageManager::removeAllOverVoltageListeners()
{
	fOverVoltageListeners.clear();
}

uint16 VoltageManager::getCurrentVoltageL2()
{
#ifndef UNIT_TEST
	uint16 spannung =0;
	bios::ADScaling::get( bios::AnalogInput::AiTerm30L2, spannung);
	return (spannung);
#else
    return fLastReportedVoltageL2;
#endif
}

} /* namespace bios */
