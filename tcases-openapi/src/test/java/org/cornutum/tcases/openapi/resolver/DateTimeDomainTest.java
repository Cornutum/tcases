//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.VarBindingBuilder;
import org.cornutum.tcases.openapi.resolver.NumberDomain.Range;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toSet;

/**
 * Runs tests for {@link DateTimeDomain}.
 */
public class DateTimeDomainTest extends ValueDomainTest
  {  
  @Test
  public void whenConstant()
    {
    // Given...
    String value = "1999-01-10T02:03:45.678+00:00";
    StringConstant domain = new DateTimeConstant( value);

    // Then...
    List<String> values = valuesOf( domain, 10);
    assertThat( "Constant values size", values.size(), is( 1));
    assertThat( "Constant value", domain.select( getResolverContext()), matches( dataValueMatcher( value, Type.STRING, "date-time")));
    assertThat( "Contains", domain.contains( value), is( true));
    assertThat( "Contains", domain.contains( ""), is( false));
    }

  @Test
  public void whenInvalidConstant()
    {
    // Given...
    String value = "1999-01-10T02:03:45.678+HH:MM";

    expectFailure( ValueDomainException.class)
      .when( () -> new DateTimeConstant( value));
    }

  @Test
  public void whenEnum()
    {
    // Given...
    String dateTime1 = "1999-01-10T02:03:45.678+00:00";
    String dateTime2 = "2000-02-13T03:04:56.789+00:00";
    List<String> dateTimes = Arrays.asList( dateTime1, dateTime2, dateTime1, dateTime2);
    DateTimeEnum domain = new DateTimeEnum( dateTimes);

    // Then...
    List<String> values = valuesOf( domain, 1000);
    assertThat( "Enum values size", values.size(), is( 1000));
    assertThat( "Enum values content", values.stream().collect( toSet()), containsMembers( Arrays.asList( dateTime1, dateTime2)));
    assertThat( "Contains", domain.contains( dateTime2), is( true));
    assertThat( "Contains", domain.contains( "2000-01-10T02:03:45.678+00:00"), is( false));
    }

  @Test
  public void whenInvalidEnum()
    {
    // Given...
    List<String> dateTimes = Arrays.asList( "1999-01-10T02:03:45.678+HH:MM");

    expectFailure( ValueDomainException.class)
      .when( () -> new DateTimeEnum( dateTimes));
    }

  @Test
  public void whenDefaultLength()
    {
    // Given...
    DateTimeDomain domain = new DateTimeDomain();

    // When...
    domain.setExcluded(
      Stream.of(
        "2019-01-10T01:02:03.456+01:01",
        "2020-02-20T01:02:03.456+02:02",
        "2021-12-21T01:02:03.456-03:03")
      .collect( toSet()));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( "2019-01-10T01:02:03.456+01:01"), is( false));
    assertThat( "Contains", domain.contains( "2019-01-09T23:01:03.456-01:00"), is( false));
    assertThat( "Contains", domain.contains( "2019-01-10T01:02:03.456-01:01"), is( true));
    assertThat( "Contains", domain.contains( "2020-02-20"), is( false));
    assertThat( "Contains", domain.contains( ""), is( false));

    // When...
    DataValue<String> value = domain.select( getResolverContext());

    // Then...
    assertThat( "Value", value, matches( dataValueMatcher( value.getValue(), Type.STRING, "date-time")));
    }

  @Test
  public void whenLengthValid()
    {
    // Given...
    DateTimeDomain domain = new DateTimeDomain();

    // When...
    domain.setLengthRange(29);
    //   MatchPatterns = Some
    //   NotMatchPatterns = Some

    // Then...
    verifyContainsValues( domain, 1000);
    }

  @Test
  public void whenLengthInvalidRange()
    {
    // Given...
    DateTimeDomain domain = new DateTimeDomain();
    domain.setLengthRange( Range.of( VarBindingBuilder.with( "Length").value( "> 29").build()));

    // When...
    DataValue<String> value = domain.select( getResolverContext());

    // Then...
    assertThat( "Invalid date-time", DateTimeConstant.isDateTime( value.getValue()), is( false));
    }

  @Test
  public void whenLengthInvalidConstant()
    {
    // Given...
    DateTimeDomain domain = new DateTimeDomain();
    domain.setLengthRange( 8);

    // When...
    DataValue<String> value = domain.select( getResolverContext());

    // Then...
    assertThat( "Invalid date-time", DateTimeConstant.isDateTime( value.getValue()), is( false));
    }
  }
