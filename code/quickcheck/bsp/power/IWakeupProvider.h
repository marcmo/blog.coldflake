/**
 * Contains interface for a class which provides informations about the wakeup
 * Reason of the last startup and provides control over the ext. Wakeupline.
 * @file 		IWakeupProvider.h
 * @ingroup		bsp
 *
 * (c) BMW AG
 */
#ifndef IWAKEUPPROVIDER_H_
#define IWAKEUPPROVIDER_H_

#include "bsp/Bsp.h"
#include "commonTypes.h"

namespace bsp
{

/**
 * @class		IWakeupProvider
 * @author		thomas.klattig
 *
 * Interface for a class which provides informations about the wakeup
 * Reason of the last startup and provides control over the ext. Wakeupline.
 */
class IWakeupProvider
{
public:

	enum ResetFlags
	{
		Reset_Unknown         = 0x00000000,
		Reset_PowerOnReset    = 0x01000000,
		Reset_ExternReset     = 0x02000000,
		Reset_LossOfLock      = 0x04000000,
		Reset_LossOfClockFPLL = 0x08000000,
		Reset_Watchdog        = 0x10000000,
		Reset_CheckStopReset  = 0x20000000,
		Reset_SwSystemReset   = 0x40000000,
		Reset_SwExternReset   = 0x80000000,
	};

	IWakeupProvider() {}

	/*!
 	*  Get wake source
	* @note The values shall reflect the wakeup reasons responsible
 	*       for most current startup.
 	* @return Wake source flags bitmask, see #wakeFlags
 	*/
	virtual uint32 getWakeupReason() = 0;

	virtual uint32 getAktivLineInfo() = 0;
	/*!
	 * @param:  wakeUpLine - WakeUpReason zum Monitoren durch wakeUpProvider
	 * @param:  aktiv 0- soll nicht ueberwacht werden, sonst überwachung Aktivieren
	 * @return: 0 -OK , others error
	 */
	virtual uint32 setMonitorWakeUp(uint32 wakeUpLine,bool aktiv)=0;

};

}

#endif /*IWAKEUPPROVIDER_H_*/
