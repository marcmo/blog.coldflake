/**
 * Contains interface to shutdown and reset the hardware.
 * @file 		IShutdownProvider.h
 * @ingroup		bsp
 *
 * (c) BMW AG
 */
#ifndef ISHUTDOWNMANAGER_H_
#define ISHUTDOWNMANAGER_H_

namespace bsp
{

/**
 * @class	IShutdownProvider
 * @author		thomas.klattig
 *
 * 	Interface to shutdown and reset the hardware.
 */
class IShutdownManager
{
public:
	/*!
	 * Switch voltage (5V0 and 3V3) physically off
	 * @note This function never returns!
	 * @note Present wakeup reasons shall be ignored!
	 * @note Time for re-power shall be below 30ms
	 */
	virtual void shutdown() = 0;
};

} /* namespace bsp */

#endif /* ISHUTDOWNPROVIDER_H_ */
