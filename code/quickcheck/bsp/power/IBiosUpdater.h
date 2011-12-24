/*
 * IBiosUpdater.h
 *
 *  Created on: Jul 16, 2010
 *      Author: mthiede
 */

#ifndef IBIOSUPDATER_H_
#define IBIOSUPDATER_H_

namespace bios
{

class IBiosUpdater
{
public:
	/**
	 * to be called cyclically for updating the bios state
	 * the parameter is the nominal cycle time
	 */
	virtual void updateCyclic(uint16 nominalCycleTimeMS) = 0;

};

}

#endif /* IBIOSUPDATER_H_ */
