/*
 * VoltageManagerTest.cpp
 *
 *  Created on: Apr 23, 2010
 *      Author: omueller
 */

#include "cppunit/extensions/HelperMacros.h"


#include "bios/voltage/VoltageManager.h"
#include "bios/voltage/AbstractCriticalVoltageListener.h"
#include "util/timeout/TimeoutManagerMock.h"

#include <string.h>

using namespace bsp;
using namespace bios;
using namespace common;
using namespace mock;


namespace test
{
typedef AbstractCriticalVoltageListener<UnderVoltageStrategy> AbstractUnderVoltageListener;
typedef AbstractCriticalVoltageListener<OverVoltageStrategy> AbstractOverVoltageListener;
class VoltageManagerTest : public CppUnit::TestFixture {

public:

	TimeoutManagerMock* 		fpTimeoutManager;
	VoltageManager*				fpVoltageManager;

	struct OverVoltageListener : public AbstractOverVoltageListener
	{
		OverVoltageListener(ITimeoutManager2& tm, uint16 level, IVoltageListener::Battery b, uint32 time, uint16 hysteresis) :
			AbstractOverVoltageListener(tm,level,b,time),
			fBattery(b),
			fOverVoltageEntered(false),
			fOverVoltageLeft(false)
		{
			setHysteresis(hysteresis);
		}
		virtual void criticalVoltageEntered()
		{ fOverVoltageEntered = true; }

		virtual void criticalVoltageLeft()
		{ fOverVoltageLeft = true; }

		IVoltageListener::Battery fBattery;
		bool fOverVoltageEntered;
		bool fOverVoltageLeft;
	};

	struct UnderVoltageListener : public AbstractUnderVoltageListener
	{
		UnderVoltageListener(ITimeoutManager2& tm, uint16 level, IVoltageListener::Battery b, uint32 debouncetime, uint16 hysteresis) :
			AbstractUnderVoltageListener(tm, level,b,debouncetime),
			fBattery(b),
			fUnderVoltageEntered(false),
			fUnderVoltageLeft(false)
		{
			setHysteresis(hysteresis);
		}

		virtual void criticalVoltageEntered()
		{ fUnderVoltageEntered = true; }

		virtual void criticalVoltageLeft()
		{ fUnderVoltageLeft = true; }

		IVoltageListener::Battery fBattery;
		bool fUnderVoltageEntered;
		bool fUnderVoltageLeft;
	};

	void setUp()
	{
		fpTimeoutManager = new TimeoutManagerMock();
		fpVoltageManager = new VoltageManager();
	}

	void tearDown()
	{
 		delete fpTimeoutManager;
		delete fpVoltageManager;
	}

	void testGetNotificationForUnderVoltage()
	{
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 0, 500);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		CPPUNIT_ASSERT(!underVoltageListener.fUnderVoltageEntered);
		fpVoltageManager->voltageChanged(5000, IVoltageListener::BATTERY_L1);
		fpTimeoutManager->alarmExpired();
		CPPUNIT_ASSERT(!underVoltageListener.fUnderVoltageEntered);
		fpVoltageManager->voltageChanged(4500, IVoltageListener::BATTERY_L1);
		fpTimeoutManager->alarmExpired();
		CPPUNIT_ASSERT(!underVoltageListener.fUnderVoltageEntered);
		fpVoltageManager->voltageChanged(4499, IVoltageListener::BATTERY_L1);
		fpTimeoutManager->alarmExpired();
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageEntered);
	}

	void testGetNoNotificationForUnderVoltageOfWrongBattery()
	{
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 0, 500);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		fpVoltageManager->voltageChanged(4499, IVoltageListener::BATTERY_L2);
		fpTimeoutManager->alarmExpired();
		CPPUNIT_ASSERT_MESSAGE("wrong battery!", !underVoltageListener.fUnderVoltageEntered);
	}

	void testGetNotificationWhenLeavingUnderVoltage()
	{
		uint16 hysteresis = 500;
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 0, hysteresis);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		CPPUNIT_ASSERT(!underVoltageListener.fUnderVoltageLeft);
		fpVoltageManager->voltageChanged(4499, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(!underVoltageListener.fUnderVoltageLeft);
		fpVoltageManager->voltageChanged(4600, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("hysteresis should prevent notification!", !underVoltageListener.fUnderVoltageLeft);
		fpVoltageManager->voltageChanged(6000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageLeft);
	}
	void testGetNotificationWhenLeavingUnderVoltage_smallHysteresis()
	{
		uint16 hysteresis = 100;
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 0, hysteresis);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		fpVoltageManager->voltageChanged(4499, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageEntered);
		underVoltageListener.fUnderVoltageEntered = false;//reset
		fpVoltageManager->voltageChanged(6000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("hysteresis too small...expecting notification!", underVoltageListener.fUnderVoltageLeft);
	}
	void testRegisterInUnderVoltage()
	{
		uint16 hysteresis = 100;
		fpVoltageManager->voltageChanged(3000, IVoltageListener::BATTERY_L1);
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 0, hysteresis);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageEntered);
	}

	void testRegisterInUnderVoltageASecondTime()
	{
		uint16 hysteresis = 100;
		fpVoltageManager->voltageChanged(12000, IVoltageListener::BATTERY_L1);
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 0, hysteresis);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		CPPUNIT_ASSERT(!underVoltageListener.fUnderVoltageEntered);
		fpVoltageManager->voltageChanged(3000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageEntered);
		underVoltageListener.fUnderVoltageEntered = false;//reset
		fpVoltageManager->removeUnderVoltageListener(underVoltageListener);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageEntered);
		fpVoltageManager->voltageChanged(12000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageLeft);
	}

	void testRegisterInOverVoltageASecondTime()
	{
		uint16 hysteresis = 100;
		fpVoltageManager->voltageChanged(12000, IVoltageListener::BATTERY_L1);
		OverVoltageListener overVoltageListener(*fpTimeoutManager, 18000, IVoltageListener::BATTERY_L1, 0, hysteresis);
		fpVoltageManager->registerOverVoltageListener(overVoltageListener);
		CPPUNIT_ASSERT(!overVoltageListener.fOverVoltageEntered);
		fpVoltageManager->voltageChanged(20000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(overVoltageListener.fOverVoltageEntered);
		overVoltageListener.fOverVoltageEntered = false;//reset
		fpVoltageManager->removeOverVoltageListener(overVoltageListener);
		fpVoltageManager->registerOverVoltageListener(overVoltageListener);
		CPPUNIT_ASSERT(overVoltageListener.fOverVoltageEntered);
		fpVoltageManager->voltageChanged(12000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(overVoltageListener.fOverVoltageLeft);
	}

	void testVoltageIncrementsSlowly()
	{
		uint16 hysteresis = 100;
		uint16 v = 3000;
		fpVoltageManager->voltageChanged(v, IVoltageListener::BATTERY_L1);
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 0, hysteresis);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		CPPUNIT_ASSERT(!underVoltageListener.fUnderVoltageLeft);
		for (;v < 5000; ++v)
		{
			fpVoltageManager->voltageChanged(v, IVoltageListener::BATTERY_L1);
		}
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageLeft);
	}

	void testGetOnlyOneNotificationWhileInUndervoltageOrHysteresis()
	{
		uint16 hysteresis = 100;
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 0, hysteresis);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		fpVoltageManager->voltageChanged(4499, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageEntered);
		underVoltageListener.fUnderVoltageEntered = false;//reset
		fpVoltageManager->voltageChanged(4550, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("left undervoltage but inside hysteresis",!underVoltageListener.fUnderVoltageEntered);
		fpVoltageManager->voltageChanged(4499, IVoltageListener::BATTERY_L1);
//		CPPUNIT_ASSERT_MESSAGE("back in undervoltage from hysteresis", !underVoltageListener.fUnderVoltageEntered);
		fpVoltageManager->voltageChanged(6000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("hysteresis too small...expecting notification!", underVoltageListener.fUnderVoltageLeft);
	}
	void testGetNotificationWhenLeavingUnderWhenExactlyMatchingHysteresis()
	{
		uint16 hysteresis = 500;
		uint16 underVoltageLevel = 4500;
		uint16 debounceTime = 0;
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, underVoltageLevel, IVoltageListener::BATTERY_L1, debounceTime, hysteresis);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		fpVoltageManager->voltageChanged(underVoltageLevel-1, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageEntered);
		fpVoltageManager->voltageChanged(underVoltageLevel, IVoltageListener::BATTERY_L1);//barely made it back from undervoltage, no notification yet
		CPPUNIT_ASSERT(!underVoltageListener.fUnderVoltageLeft);
		fpVoltageManager->voltageChanged(underVoltageLevel + hysteresis + 1, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("expecting notification!", underVoltageListener.fUnderVoltageLeft);
		underVoltageListener.fUnderVoltageLeft = false;
		fpVoltageManager->voltageChanged(underVoltageLevel + hysteresis + 100, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("no new notification expected!", !underVoltageListener.fUnderVoltageLeft);
	}

	void testGetNotificationWhenLeavingOverWhenExactlyMatchingHysteresis()
	{
		uint16 hysteresis = 500;
		uint16 overVoltageLevel = 14500;
		uint16 debounceTime = 0;
		OverVoltageListener overVoltageListener(*fpTimeoutManager, overVoltageLevel, IVoltageListener::BATTERY_L1, debounceTime, hysteresis);
		fpVoltageManager->registerOverVoltageListener(overVoltageListener);
		fpVoltageManager->voltageChanged(overVoltageLevel+1, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(overVoltageListener.fOverVoltageEntered);
		fpVoltageManager->voltageChanged(overVoltageLevel, IVoltageListener::BATTERY_L1);//barely made it back from overvoltage, no notification yet
		CPPUNIT_ASSERT(!overVoltageListener.fOverVoltageLeft);
		fpVoltageManager->voltageChanged(overVoltageLevel - hysteresis, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("expecting no notification!", !overVoltageListener.fOverVoltageLeft);
		fpVoltageManager->voltageChanged(overVoltageLevel - hysteresis - 1, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("expecting notification!", overVoltageListener.fOverVoltageLeft);
		overVoltageListener.fOverVoltageLeft = false;
		fpVoltageManager->voltageChanged(overVoltageLevel - hysteresis - 100, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("no new notification expected!", !overVoltageListener.fOverVoltageLeft);
	}

	void testGetNotificationForOverVoltage()
	{
		OverVoltageListener overVoltageListener(*fpTimeoutManager, 16000, IVoltageListener::BATTERY_L1, 0, 500);
		fpVoltageManager->registerOverVoltageListener(overVoltageListener);
		fpVoltageManager->voltageChanged(15000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(!overVoltageListener.fOverVoltageEntered);
		fpVoltageManager->voltageChanged(16000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(!overVoltageListener.fOverVoltageEntered);
		fpVoltageManager->voltageChanged(16001, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(overVoltageListener.fOverVoltageEntered);
	}

	void testGetNotificationWhenLeavingOverVoltage()
	{
		uint16 hysteresis = 500;
		OverVoltageListener overVoltageListener(*fpTimeoutManager, 16000, IVoltageListener::BATTERY_L1, 0, hysteresis);
		fpVoltageManager->registerOverVoltageListener(overVoltageListener);
		fpVoltageManager->voltageChanged(16001, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(!overVoltageListener.fOverVoltageLeft);
		fpVoltageManager->voltageChanged(15900, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("hysteresis should prevent notification!", !overVoltageListener.fOverVoltageLeft);
		fpVoltageManager->voltageChanged(15000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(overVoltageListener.fOverVoltageLeft);
	}

	void testGetNotificationWhenLeavingOverVoltage_smallHysteresis()
	{
		uint16 hysteresis = 100;
		OverVoltageListener overVoltageListener(*fpTimeoutManager, 16000, IVoltageListener::BATTERY_L1, 0, hysteresis);
		fpVoltageManager->registerOverVoltageListener(overVoltageListener);
		fpVoltageManager->voltageChanged(16001, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(overVoltageListener.fOverVoltageEntered);
		fpVoltageManager->voltageChanged(15000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("hysteresis was too small, immediate notification!", overVoltageListener.fOverVoltageLeft);
	}
	void testLeavingAndReenteringOverVoltage()
	{
		uint16 hysteresis = 500;
		OverVoltageListener overVoltageListener(*fpTimeoutManager, 16000, IVoltageListener::BATTERY_L1, 0, hysteresis);
		fpVoltageManager->registerOverVoltageListener(overVoltageListener);
		fpVoltageManager->voltageChanged(16001, IVoltageListener::BATTERY_L1);
		fpTimeoutManager->alarmExpired();
		CPPUNIT_ASSERT(overVoltageListener.fOverVoltageEntered);
		overVoltageListener.fOverVoltageEntered = false; //reset conditions
		fpVoltageManager->voltageChanged(16200, IVoltageListener::BATTERY_L1);
		fpTimeoutManager->alarmExpired();
		CPPUNIT_ASSERT_MESSAGE("no new notification wanted", !overVoltageListener.fOverVoltageLeft);
		fpVoltageManager->voltageChanged(15999, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(!overVoltageListener.fOverVoltageLeft);
		fpVoltageManager->voltageChanged(15000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(overVoltageListener.fOverVoltageLeft);
	}
	void testFirstValueIsOverVoltage()
	{
		OverVoltageListener overVoltageListener(*fpTimeoutManager, 16000, IVoltageListener::BATTERY_L1, 0, 500);
		fpVoltageManager->registerOverVoltageListener(overVoltageListener);
		fpVoltageManager->voltageChanged(17000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(overVoltageListener.fOverVoltageEntered);
	}
	void testFirstValueIsUnderVoltage()
	{
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 0, 500);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		fpVoltageManager->voltageChanged(3000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageEntered);
	}

	void testRegisterForDifferentBatterieLevelsL1_overVoltage()
	{
		OverVoltageListener ovl(*fpTimeoutManager, 15000, IVoltageListener::BATTERY_L1, 0, 500);
		fpVoltageManager->registerOverVoltageListener(ovl);
		fpVoltageManager->voltageChanged(16000, IVoltageListener::BATTERY_L2);
		CPPUNIT_ASSERT_MESSAGE("should not be notified, wrong battery", !ovl.fOverVoltageEntered);
	}
	void testRegisterForDifferentBatterieLevelsL2_overVoltage()
	{
		OverVoltageListener ovl(*fpTimeoutManager, 15000, IVoltageListener::BATTERY_L2, 0, 500);
		fpVoltageManager->registerOverVoltageListener(ovl);
		fpVoltageManager->voltageChanged(16000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("should not be notified, wrong battery", !ovl.fOverVoltageEntered);
	}
	void testRegisterForDifferentBatterieLevelsL1_underVoltage()
	{
		UnderVoltageListener uvl(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 0, 500);
		fpVoltageManager->registerUnderVoltageListener(uvl);
		fpVoltageManager->voltageChanged(3000, IVoltageListener::BATTERY_L2);
		CPPUNIT_ASSERT_MESSAGE("should not be notified, wrong battery", !uvl.fUnderVoltageEntered);
	}
	void testRegisterForDifferentBatterieLevelsL2_underVoltage()
	{
		UnderVoltageListener uvl(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L2, 0, 500);
		fpVoltageManager->registerUnderVoltageListener(uvl);
		fpVoltageManager->voltageChanged(3000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("should not be notified, wrong battery", !uvl.fUnderVoltageEntered);
	}
	void testDebouncedNotificationUnderVoltage()
	{
		UnderVoltageListener uvl(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 100, 500);
		fpVoltageManager->registerUnderVoltageListener(uvl);
		fpVoltageManager->voltageChanged(3000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("should not be notified, time was not debounced", !uvl.fUnderVoltageEntered);
		fpTimeoutManager->alarmExpired();
		CPPUNIT_ASSERT_MESSAGE("now time was debounced", uvl.fUnderVoltageEntered);
	}
	void testDebouncedNotificationOverVoltage()
	{
		OverVoltageListener ovl(*fpTimeoutManager, 16000, IVoltageListener::BATTERY_L1, 100, 500);
		fpVoltageManager->registerOverVoltageListener(ovl);
		fpVoltageManager->voltageChanged(17000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("should not be notified, time was not debounced", !ovl.fOverVoltageEntered);
		fpTimeoutManager->alarmExpired();
		CPPUNIT_ASSERT_MESSAGE("now time was debounced", ovl.fOverVoltageEntered);
	}
	void testShortUnderVoltageWithoutNotification()
	{
		UnderVoltageListener uvl(*fpTimeoutManager, 4500, IVoltageListener::BATTERY_L1, 100, 500);
		fpVoltageManager->registerUnderVoltageListener(uvl);
		fpVoltageManager->voltageChanged(3000, IVoltageListener::BATTERY_L1);
		//now current goes up again
		fpVoltageManager->voltageChanged(5000, IVoltageListener::BATTERY_L1);
		fpTimeoutManager->alarmExpired();
		CPPUNIT_ASSERT_MESSAGE("should not be notified, time was too short", !uvl.fUnderVoltageEntered);
	}
	void testShortOverVoltageWithoutNotification()
	{
		OverVoltageListener ovl(*fpTimeoutManager, 16000, IVoltageListener::BATTERY_L1, 100, 500);
		fpVoltageManager->registerOverVoltageListener(ovl);
		fpVoltageManager->voltageChanged(17000, IVoltageListener::BATTERY_L1);
		//now current goes down again
		fpVoltageManager->voltageChanged(12000, IVoltageListener::BATTERY_L1);
		fpTimeoutManager->alarmExpired();
		CPPUNIT_ASSERT_MESSAGE("should not be notified, time was too short", !ovl.fOverVoltageEntered);
	}

	void testGetCurrentVoltage()
	{
		uint16 levelA = 6000;
		uint16 levelB = 8000;
		uint16 levelC = 9000;
		fpVoltageManager->voltageChanged(levelA, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_EQUAL_MESSAGE("should be updated (L1)", levelA, fpVoltageManager->getCurrentVoltage());
		fpVoltageManager->voltageChanged(levelB, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_EQUAL_MESSAGE("should be updated (L1)", levelB, fpVoltageManager->getCurrentVoltage());
		fpVoltageManager->voltageChanged(levelC, IVoltageListener::BATTERY_L2);
		CPPUNIT_ASSERT_EQUAL_MESSAGE("should NOT be updated (was L2)", levelB, fpVoltageManager->getCurrentVoltage());
	}

	void testStartUpInUnderVoltage()
	{
		fpVoltageManager->voltageChanged(5000, IVoltageListener::BATTERY_L1);
		uint16 hysteresis = 100;
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, 6000, IVoltageListener::BATTERY_L1, 0, hysteresis);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageEntered);
		fpVoltageManager->voltageChanged(6200, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageLeft);
	}

	void testWishWashUseCase()
	{
//		Rte_Call_underVoltage6000WwFs_registerUnderVoltageListener(RTESelf, 5750, 100, VOLTAGE_BATTERY_ONE, undervoltageHysteresis);
		uint16 hysteresis = 500;
		UnderVoltageListener underVoltageListener(*fpTimeoutManager, 5750, IVoltageListener::BATTERY_L1, 0, hysteresis);
		//start up in undervoltage
		fpVoltageManager->voltageChanged(5885, IVoltageListener::BATTERY_L1);
		fpVoltageManager->registerUnderVoltageListener(underVoltageListener);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageEntered);
		CPPUNIT_ASSERT(!underVoltageListener.fUnderVoltageLeft);
		fpVoltageManager->voltageChanged(5995, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(!underVoltageListener.fUnderVoltageLeft);
		fpVoltageManager->voltageChanged(4600, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT_MESSAGE("hysteresis should prevent notification!", !underVoltageListener.fUnderVoltageLeft);
		fpVoltageManager->voltageChanged(6000, IVoltageListener::BATTERY_L1);
		CPPUNIT_ASSERT(underVoltageListener.fUnderVoltageLeft);
	}

	CPPUNIT_TEST_SUITE( VoltageManagerTest );

			CPPUNIT_TEST( testGetNotificationForUnderVoltage );
			CPPUNIT_TEST( testGetNoNotificationForUnderVoltageOfWrongBattery );
			CPPUNIT_TEST( testGetNotificationWhenLeavingUnderVoltage );
			CPPUNIT_TEST( testGetNotificationWhenLeavingUnderVoltage_smallHysteresis );
			CPPUNIT_TEST( testRegisterInUnderVoltage );
			CPPUNIT_TEST( testRegisterInUnderVoltageASecondTime );
			CPPUNIT_TEST( testRegisterInOverVoltageASecondTime );
			CPPUNIT_TEST( testVoltageIncrementsSlowly );
			CPPUNIT_TEST( testGetOnlyOneNotificationWhileInUndervoltageOrHysteresis );
			CPPUNIT_TEST( testGetNotificationWhenLeavingUnderWhenExactlyMatchingHysteresis );
			CPPUNIT_TEST( testGetNotificationWhenLeavingOverWhenExactlyMatchingHysteresis );
			CPPUNIT_TEST( testGetNotificationForOverVoltage );
			CPPUNIT_TEST( testGetNotificationWhenLeavingOverVoltage );
			CPPUNIT_TEST( testGetNotificationWhenLeavingOverVoltage_smallHysteresis );
			CPPUNIT_TEST( testLeavingAndReenteringOverVoltage );
			CPPUNIT_TEST( testFirstValueIsUnderVoltage );
			CPPUNIT_TEST( testFirstValueIsOverVoltage );
			CPPUNIT_TEST( testRegisterForDifferentBatterieLevelsL1_overVoltage );
			CPPUNIT_TEST( testRegisterForDifferentBatterieLevelsL2_overVoltage );
			CPPUNIT_TEST( testRegisterForDifferentBatterieLevelsL1_underVoltage );
			CPPUNIT_TEST( testRegisterForDifferentBatterieLevelsL2_underVoltage );
			CPPUNIT_TEST( testDebouncedNotificationUnderVoltage );
			CPPUNIT_TEST( testDebouncedNotificationOverVoltage );
			CPPUNIT_TEST( testShortUnderVoltageWithoutNotification );
			CPPUNIT_TEST( testShortOverVoltageWithoutNotification );
			CPPUNIT_TEST( testGetCurrentVoltage );
			CPPUNIT_TEST( testStartUpInUnderVoltage );
			//FIXME: test notification of undervoltagelistener when recovering:
			// V0 = undervoltage threshold
			// V1 = V0 + hysteresis
			// oldvoltage < V0, newvoltage < V0 => nothing happens
			// oldvoltage < V0, V0 < newvoltage < V1 => callback
			// oldvoltage < V0, newvoltage < V0 < V1 => callback2
			// ...


	CPPUNIT_TEST_SUITE_END();

};

CPPUNIT_TEST_SUITE_REGISTRATION(VoltageManagerTest);
} //namespace test
