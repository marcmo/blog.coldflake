
#ifndef IBSPSLEEP_H_
#define IBSPSLEEP_H_

#include "commonTypes.h"

namespace bsp
{

class IBSPSleep
{
public :
/*!
 * ForceShutdown : Post Task
 */
	virtual void postForceShutdown(uint32 hwVersion) = 0;
/*!
 * ForceShutdown : Pre Task
 */
	virtual void preForceShutdown(uint32 hwVersion) = 0;
/*!
 * @called :  from "go to Sleep" to make BSP raedy for Sleep
 */
	virtual bool goToSleep(uint32 hwVersion , uint32 pseudoSleep) = 0;
/*!
 * @called : im Sleep im Case Cyclik WakeUp ..
 */
	virtual void sleepTask(uint32 hwVersion , uint32 pseudoSleep) = 0;
/*!
 * @called : im Sleep
 * @ret : 0 -> we can sleep , !0 -> we have to startUp
 */
	virtual uint32 checkWakeUp(uint32 hwVersion , uint32 pseudoSleep) = 0;
/*!
 * @called : after sleep .For BSP  reStart after Sleep
 */
	virtual bool startUpAfterSleep(uint32 hwVersion , uint32 pseudoSleep) = 0;
/*!
 * @called :
 * @disc   : convert WakeUp HW Edge to application WakeUp reasons
 */
	virtual uint32 kWupEdge2WakeUpResone(uint32 hwVersion,uint32 wakeUpReson) = 0;
/*!
 * @called :
 * @disc   : for Start BSP for Full
 */
	virtual void fullPowerUp(uint32 hwVersion) = 0;
/*!
 * @called :
 * @disc   : for PreSleep
 */
	virtual void startPreSleep(uint32 hwVersion) = 0;

};

} //namespace bsp
#endif /* IBSPSLEEP_H_ */
