#ifndef VOLTAGEMANAGER_H_
#define VOLTAGEMANAGER_H_

#include "bsp/power/IVoltageManager.h"
#include "bsp/power/IVoltageListener.h"
#include "bsp/power/ICriticalVoltageListener.h"
#include "util/SLinkedListSet.h"

namespace bsp
{
class AbstractPowerStateListener;
}

namespace bios
{

class VoltageManager :
    public bsp::IVoltageManager,
    public bsp::IVoltageListener
{
public:
    enum { NORMAL_VOLTAGE = 12000 };

    VoltageManager();
    /** @implements IVoltageManager */
    virtual void registerUnderVoltageListener(bsp::ICriticalVoltageListener&);
    virtual void registerOverVoltageListener(bsp::ICriticalVoltageListener&);
    virtual void removeUnderVoltageListener(bsp::ICriticalVoltageListener&);
    virtual void removeOverVoltageListener(bsp::ICriticalVoltageListener&);
    virtual uint16 getCurrentVoltage(); //returns millivolts
    virtual uint16 getCurrentVoltageL2(); //returns millivolts
    virtual void removeAllUnderVoltageListeners();
    virtual void removeAllOverVoltageListeners();

    /** @implements IVoltageListener */
    void voltageChanged(uint16 voltage, Battery battery);

private:

	void setNotifyFlagForListeners(uint16 oldVoltage, uint16 newVoltage, Battery b);

    typedef SLinkedListSet<bsp::ICriticalVoltageListener>  tUnderVoltageListeners;
    typedef SLinkedListSet<bsp::ICriticalVoltageListener>  tOverVoltageListeners;
    tUnderVoltageListeners  	fUnderVoltageListeners;
    tOverVoltageListeners  		fOverVoltageListeners;
    uint16             			fLastReportedVoltageL1;
    uint16             			fLastReportedVoltageL2;
    bool fL1_l2_initialized;
};

} //namespace bios

#endif /* VOLTAGEMANAGER_H_ */
