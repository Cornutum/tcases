//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;
import org.cornutum.tcases.openapi.FormattedString;
import org.cornutum.tcases.util.MapBuilder;

import org.junit.Test;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
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
        new MapBuilder<String,DataValue<?>>()
        .put( "a", new DecimalValue( new BigDecimal( "0.0")))
        .build()),
      is( true));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .put( "A", new DecimalValue( new BigDecimal( "0.0")))
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .put( "a", new DecimalValue( new BigDecimal( "-1.0")))
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .put( "a", new StringValue( "0.0"))
        .build()),
      is( false));
    }

  @Test
  public void whenManyProperties()
    {
    // Given...
    ObjectDomain domain = new ObjectDomain();

    // When...
    MapBuilder<String,ValueDomain<?>> properties = new MapBuilder<String,ValueDomain<?>>();
    domain.setPropertyDomains(
      properties
      .put( dateString( 2020, 1, 2), new AsciiStringDomain( 4))
      .put( dateString( 2020, 1, 4), new IntegerDomain( 0, 100))
      .put( dateString( 2020, 1, 6), new DecimalDomain( -1.25, 1.25))
      .build());

    domain.setAdditionalPropertyCount( new IntegerDomain( 0, 4));
    domain.setAdditionalPropertyNames( new DateDomain( date( 2020, 1, 1), date( 2020, 1, 7)));

    // Then...
    verifyContainsValues( domain, 1000);

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .put( dateString( 2020, 1, 2), new StringValue( "ABCD"))
        .put( dateString( 2020, 1, 4), new IntegerValue( 0))
        .put( dateString( 2020, 1, 6), new DecimalValue( new BigDecimal( "-1.00")))
        .build()),
      is( true));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .put( dateString( 2020, 1, 2), new StringValue( "0"))
        .put( dateString( 2020, 1, 4), new IntegerValue( 100))
        .put( dateString( 2020, 1, 6), new DecimalValue( new BigDecimal( "1.25")))
        .put( dateString( 2020, 1, 1), new StringValue( "Howdy!"))
        .put( dateString( 2020, 1, 3), new IntegerValue( -987))
        .put( dateString( 2020, 1, 5), new ObjectValue( emptyMap()))
        .put( dateString( 2020, 1, 7), new DecimalValue( BigDecimal.ZERO))
        .build()),
      is( true));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .put( dateString( 2020, 1, 2), new StringValue( "0"))
        .put( dateString( 2020, 1, 4), new IntegerValue( 0))
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .put( dateString( 2020, 1, 2), new StringValue( "ABCD"))
        .put( dateString( 2020, 1, 4), new IntegerValue( 1))
        .put( dateString( 2020, 1, 6), new StringValue( "-1.00"))
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .put( dateString( 2020, 1, 2), new StringValue( "ABCD"))
        .put( dateString( 2020, 1, 4), new IntegerValue( 0))
        .put( dateString( 2020, 1, 6), new DecimalValue( new BigDecimal( "0.12")))
        .put( "myString", new StringValue( "?"))
        .build()),
      is( false));
    }

  @Test
  public void whenNoAdditionalProperties()
    {
    // Given...
    ObjectDomain domain = new ObjectDomain();

    // When...
    MapBuilder<String,ValueDomain<?>> properties = new MapBuilder<String,ValueDomain<?>>();
    domain.setPropertyDomains( properties.put( "alpha", new AsciiStringDomain( 4)).build());

    // Then...
    verifyContainsValues( domain, 1000);

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .put( "alpha", new StringValue( "ABCD"))
        .build()),
      is( true));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .build()),
      is( false));

    assertThat(
      "Contains",
      domain.contains(
        new MapBuilder<String,DataValue<?>>()
        .put( "alpha", new StringValue( "ABCD"))
        .put( "bravo", new IntegerValue( 0))
        .build()),
      is( false));
    }

  @Test
  public void whenInvalidPropertyName()
    {
    // Given...
    ObjectDomain domain = new ObjectDomain( Characters.TOKEN);

    // When...
    Map<String,ValueDomain<?>> properties = new MapBuilder<String,ValueDomain<?>>().put( "<id>", new AsciiStringDomain( 4)).build();

    // Then...
    expectFailure( ValueDomainException.class)
      .when( () -> domain.setPropertyDomains( properties));
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
