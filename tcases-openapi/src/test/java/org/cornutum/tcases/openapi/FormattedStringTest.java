//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
import static java.util.Calendar.*;

/**
 * Runs tests for {@link FormattedString} methods.
 */
public class FormattedStringTest
  {
  @Test
  public void whenFormatDate()
    {
    // Given...
    Date date =
      new Calendar.Builder()
      .setDate( 2019, 0, 1)
      .build()
      .getTime();
    
    // When...
    FormattedString formatted = FormattedString.of( "date", date);
    
    // Then...
    assertThat( "Date string", formatted.toString(), is( "2019-01-01"));
    }
  
  @Test
  public void whenFormatDateTime()
    {
    // Given...
    Calendar calendar =
      new Calendar.Builder()
      .setDate( 2019, 7, 9)
      .setTimeOfDay( 23, 59, 59, 876)
      .setTimeZone( TimeZone.getTimeZone( "GMT"))
      .build();

    Date date = calendar.getTime();
    int localZoneOffset = TimeZone.getDefault().getOffset( System.currentTimeMillis()) / 60000;
      
    // When...
    FormattedString formatted = FormattedString.of( "date-time", date);
    
    // Then...
    assertThat( "Date string", formatted.toString(), is( toDateTimeString( calendar, localZoneOffset)));
    }
  
  @Test
  public void whenFormatByte()
    {
    // Given...
    byte[] bytes = "Hello, world".getBytes();
    
    // When...
    FormattedString formatted = FormattedString.of( "byte", bytes);
    
    // Then...
    assertThat( "Bytes", formatted.toString(), is( "SGVsbG8sIHdvcmxk"));
    }

  /**
   * Returns the expected string representation of the given time value, using the given local time zone offset.
   */
  private String toDateTimeString( Calendar calendar, int localZoneOffset)
    {
    int zonedHrMin = 60 * calendar.get( HOUR_OF_DAY) + calendar.get( MINUTE) + localZoneOffset;
    int zonedHr = zonedHrMin / 60;
    int zonedMin = zonedHrMin % 60;

    return
      String.format(
        "%s-%02d-%02dT%02d:%02d:%02d.%s%s%02d:%02d",
        calendar.get( YEAR),
        calendar.get( MONTH) + 1,
        calendar.get( DAY_OF_MONTH),
        zonedHr,
        zonedMin,
        calendar.get( SECOND),
        calendar.get( MILLISECOND),
        localZoneOffset >= 0? "+" : "-",
        Math.abs( localZoneOffset / 60),
        Math.abs( localZoneOffset % 60));

    }

  }
