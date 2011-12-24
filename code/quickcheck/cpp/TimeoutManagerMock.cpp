#include "TimeoutManagerMock.h"
#include "util/timeout/AbstractTimeout.h"
#include <stdio.h>

using namespace common;

namespace mock
{

TimeoutManagerMock::TimeoutManagerMock()
{
}

ITimeoutManager2::ErrorCode TimeoutManagerMock::set(
	AbstractTimeout& timeout,
	uint32 time,
	bool cyclic)
{
	if (time == 0)
	{
		return ITimeoutManager2::TIMEOUT_INVALID_VALUE;
	}
	if (fActiveTimeouts.contains(timeout))
	{
		if (!timeout.fIsCancelled && (timeout.fRemainingTime > 0 || timeout.fIsCyclic))
		{
			return ITimeoutManager2::TIMEOUT_ALREADY_SET;
		}
	}
	timeout.fTimeout = time;
	timeout.fRemainingTime = time;
	timeout.fIsCyclic = cyclic;
	timeout.fIsCancelled = false;
	fActiveTimeouts.push_back(timeout);
	return ITimeoutManager2::TIMEOUT_OK;
}

void TimeoutManagerMock::alarmExpired()
{
	if (fActiveTimeouts.size() > 0)
	{
		const AbstractTimeout* pMinimum = findMinimumTimeout();
		uint32 expiredTime = pMinimum->fRemainingTime;
		adjustTimeouts(expiredTime);
	}
	else
	{
	    // printf("Called TimeoutManagerMock::alarmExpired() although timeout queue is empty!\n");
	}
}

void TimeoutManagerMock::cancel(AbstractTimeout& timeout)
{
	timeout.fIsCancelled = true;
	timeout.fRemainingTime = 0;
	if (fActiveTimeouts.contains(timeout))
	{
		fActiveTimeouts.remove(timeout);
	}
}

void TimeoutManagerMock::adjustTimeouts(uint32 time)
{
	tTimeoutList::iterator itr = fActiveTimeouts.begin();
	while (itr != fActiveTimeouts.end())
	{
		if (!itr->fIsCancelled)
		{
			if (itr->fRemainingTime < time)
			{
				printf("%d < %d\n", itr->fRemainingTime, time);
			}
			if (itr->fRemainingTime <= time)
			{//timeout is expired
				itr->fRemainingTime = 0;
				itr->expired(AbstractTimeout::TimeoutExpiredActions(*itr));
				if ((itr->fIsCancelled)
					|| ((itr->fRemainingTime == 0) && (!itr->fIsCyclic)))
				{//timeout has been cancelled in callback and not been reset
					itr = fActiveTimeouts.erase(itr);
				}
				else
				{
					if (itr->fIsCyclic)
					{
						itr->fRemainingTime = itr->fTimeout;
					}
					++itr;
				}
			}
			else
			{//adjust timeout
				itr->fRemainingTime -= time;
				++itr;
			}
		}
		else
		{
			itr = fActiveTimeouts.erase(itr);
		}
	}
}

const AbstractTimeout* TimeoutManagerMock::findMinimumTimeout() const
{
	const AbstractTimeout* pMinimum = 0L;
	tTimeoutList::const_iterator e_itr = fActiveTimeouts.end();
	for (tTimeoutList::const_iterator itr = fActiveTimeouts.begin(); itr != e_itr; ++itr)
	{
		if(!itr->fIsCancelled && (!pMinimum || (itr->fRemainingTime < pMinimum->fRemainingTime)))
		{
			pMinimum = itr.operator->();
		}
	}
	if(pMinimum)
	{
		if(pMinimum->fRemainingTime <= 0)
		{
			assert(pMinimum->fRemainingTime > 0);
		}
	}
	return pMinimum;
}

uint32 TimeoutManagerMock::getRemainingTime(const common::AbstractTimeout& timeout) const
{
	return timeout.fRemainingTime;
}

} //namespace mock
