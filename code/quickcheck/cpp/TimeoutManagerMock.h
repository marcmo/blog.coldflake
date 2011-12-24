/**
 * Includes TimeoutManagerMock.
 * @file	TimeoutManagerMock.h
 * @ingroup	mock
 *
 * (c) BMW CarIT GmbH
 */
#ifndef TIMEOUTMANAGERMOCK_H_
#define TIMEOUTMANAGERMOCK_H_

#include "util/timeout/ITimeoutManager2.h"
#include "util/SLinkedListSet.h"

namespace mock
{

/**
 * @author	matthias.kessler
 *
 * @see	ITimeoutManager
 */
class TimeoutManagerMock :
	public common::ITimeoutManager2
{
public:
	TimeoutManagerMock();

	/**
	 * @see	ITimeoutManager2::set()
	 */
	virtual ErrorCode set(
		common::AbstractTimeout& timeout,
		uint32 time,
		bool cyclic = false);

	/**
	 * @see	ITimeoutManager2::cancel()
	 */
	virtual void cancel(common::AbstractTimeout& timeout);

	/**
	 * Method to be called when the OSEK alarm expires so that the TimeoutManger
	 * can notify the registered listener
	 */
	virtual void alarmExpired();

	virtual void init() {}

	virtual void shutdown() {}

	virtual uint32 getRemainingTime(const common::AbstractTimeout& timeout) const;


private:
	/** type of pool of Timeout objects */
	typedef SLinkedListSet<common::AbstractTimeout>
									tTimeoutList;

	const common::AbstractTimeout* findMinimumTimeout() const;
public:
	/**
	 * Decrements all active timeouts by a given time
	 * @param	time	time in ms to adjust
	 */
	void adjustTimeouts(uint32 time);
private:
	/** pool of Timeout objects to use */
	tTimeoutList				fActiveTimeouts;
};

} //namespace mock

#endif /*TIMEOUTMANAGERMOCK_H_*/
