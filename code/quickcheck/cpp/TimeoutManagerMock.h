#ifndef TIMEOUTMANAGERMOCK_H_
#define TIMEOUTMANAGERMOCK_H_

#include "util/timeout/ITimeoutManager2.h"
#include "util/SLinkedListSet.h"

namespace mock
{

class TimeoutManagerMock :
	public common::ITimeoutManager2
{
public:
	TimeoutManagerMock();

	virtual ErrorCode set(
		common::AbstractTimeout& timeout,
		uint32 time,
		bool cyclic = false);

	virtual void cancel(common::AbstractTimeout& timeout);

	virtual void alarmExpired();

	virtual void init() {}

	virtual void shutdown() {}

	virtual uint32 getRemainingTime(const common::AbstractTimeout& timeout) const;


private:
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
	tTimeoutList				fActiveTimeouts;
};

} //namespace mock

#endif /*TIMEOUTMANAGERMOCK_H_*/
