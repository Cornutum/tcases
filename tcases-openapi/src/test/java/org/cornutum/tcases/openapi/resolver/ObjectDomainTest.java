//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.FormattedString;
import org.cornutum.tcases.util.MapBuilder;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;
import static java.util.Collections.emptyMap;

/**
 * Runs tests for {@link ObjectDomain}.
 */
public class ObjectDomainTest extends ValueDomainTest
  {
  @Test
  public void whenAdditionalPropertiesOnly()
    {
    // Given...
    ObjectDomain domain = new ObjectDomain();

    // When...
    domain.setAdditionalPropertyCount( new IntegerConstant( 1));
    domain.setAdditionalPropertyValues( new DecimalDomain( 0.0, 1.0));

    // Then...
    verifyContainsValues( domain, 1000);

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .put( "a", new BigDecimal( "0.0"))
        .build()),
      is( true));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .put( "A", new BigDecimal( "0.0"))
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .put( "a", new BigDecimal( "-1.0"))
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .put( "a", "0.0")
        .build()),
      is( false));
    }

  @Test
  public void whenManyProperties()
    {
    // Given...
    ObjectDomain domain = new ObjectDomain();

    // When...
    domain.getPropertyDomains().put(dateString( 2020, 1, 2), new AsciiStringDomain( 4));
    domain.getPropertyDomains().put(dateString( 2020, 1, 4), new IntegerDomain( 0, 100));
    domain.getPropertyDomains().put(dateString( 2020, 1, 6), new DecimalDomain( -1.25, 1.25));

    domain.setAdditionalPropertyCount( new IntegerDomain( 0, 4));
    domain.setAdditionalPropertyNames( new DateDomain( date( 2020, 1, 1), date( 2020, 1, 7)));

    // Then...
    verifyContainsValues( domain, 1000);

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .put( dateString( 2020, 1, 2), "ABCD")
        .put( dateString( 2020, 1, 4), 0)
        .put( dateString( 2020, 1, 6), new BigDecimal( "-1.00"))
        .build()),
      is( true));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .put( dateString( 2020, 1, 2), "0")
        .put( dateString( 2020, 1, 4), 100)
        .put( dateString( 2020, 1, 6), new BigDecimal( "1.25"))
        .put( dateString( 2020, 1, 1), "Howdy!")
        .put( dateString( 2020, 1, 3), -987)
        .put( dateString( 2020, 1, 5), emptyMap())
        .put( dateString( 2020, 1, 7), BigDecimal.ZERO)
        .build()),
      is( true));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .put( dateString( 2020, 1, 2), "0")
        .put( dateString( 2020, 1, 4), 0)
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .put( dateString( 2020, 1, 2), "ABCD")
        .put( dateString( 2020, 1, 4), 1)
        .put( dateString( 2020, 1, 6), "-1.00")
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .put( dateString( 2020, 1, 2), "ABCD")
        .put( dateString( 2020, 1, 4), 0)
        .put( dateString( 2020, 1, 6), new BigDecimal( "0.12"))
        .put( "myString", "?")
        .build()),
      is( false));
    }

  @Test
  public void whenNoAdditionalProperties()
    {
    // Given...
    ObjectDomain domain = new ObjectDomain();

    // When...
    domain.getPropertyDomains().put( "alpha", new AsciiStringDomain( 4));

    // Then...
    verifyContainsValues( domain, 1000);

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .put( "alpha", "ABCD")
        .build()),
      is( true));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,Object>()
        .put( "alpha", "ABCD")
        .put( "bravo", 0)
        .build()),
      is( false));
    }

  /**
   * Returns the given date as a string.
   */
  private String dateString( int year, int month, int day)
    {
    return FormattedString.getDateFormat().format( date( year, month, day));
    }

  /**
   * Returns the given date.
   */
  private Date date( int year, int month, int day)
    {
    return new Calendar.Builder().setDate( year, month, day).build().getTime();
    }

  }
