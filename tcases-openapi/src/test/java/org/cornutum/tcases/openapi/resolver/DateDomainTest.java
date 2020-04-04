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
 * Runs tests for {@link DateDomain}.
 */
public class DateDomainTest extends ValueDomainTest
  {
  @Test
  public void whenConstant()
    {
    // Given...
    String value = "1999-01-10";
    StringConstant domain = new DateConstant( value);

    // Then...
    List<String> values = valuesOf( domain, 10);
    assertThat( "Constant values size", values.size(), is( 1));
    assertThat( "Constant value", domain.select( getResolverContext()), matches( dataValueMatcher( value, Type.STRING, "date")));
    assertThat( "Contains", domain.contains( value), is( true));
    assertThat( "Contains", domain.contains( ""), is( false));
    }

  @Test
  public void whenInvalidConstant()
    {
    // Given...
    String value = "1999/01/10";

    expectFailure( ValueDomainException.class)
      .when( () -> new DateConstant( value));
    }

  @Test
  public void whenEnum()
    {
    // Given...
    List<String> dates = Arrays.asList( "1999-01-10", "2000-02-13", "1999-01-10");
    DateEnum domain = new DateEnum( dates);

    // Then...
    List<String> values = valuesOf( domain, 1000);
    assertThat( "Enum values size", values.size(), is( 1000));
    assertThat( "Enum values content", values.stream().collect( toSet()), containsMembers( Arrays.asList( "1999-01-10", "2000-02-13")));
    assertThat( "Contains", domain.contains( "2000-02-13"), is( true));
    assertThat( "Contains", domain.contains( "2001-02-13"), is( false));
    }

  @Test
  public void whenInvalidEnum()
    {
    // Given...
    List<String> dates = Arrays.asList( "1999/01/10");

    expectFailure( ValueDomainException.class)
      .when( () -> new DateEnum( dates));
    }

  @Test
  public void whenDefaultLength()
    {
    // Given...
    DateDomain domain = new DateDomain();

    // When...
    domain.setExcluded( Stream.of( "2019-01-10", "2020-02-20", "2021-12-21").collect( toSet()));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( "2019/01/10"), is( false));
    assertThat( "Contains", domain.contains( "2019-02-20"), is( true));
    assertThat( "Contains", domain.contains( "2020-02-20"), is( false));
    assertThat( "Contains", domain.contains( ""), is( false));

    // When...
    DataValue<String> value = domain.select( getResolverContext());

    // Then...
    assertThat( "Value", value, matches( dataValueMatcher( value.getValue(), Type.STRING, "date")));
    }

  @Test
  public void whenLengthValid()
    {
    // Given...
    DateDomain domain = new DateDomain();

    // When...
    domain.setLengthRange(10);
    //   MatchPatterns = Some
    //   NotMatchPatterns = Some

    // Then...
    verifyContainsValues( domain, 1000);
    }

  @Test
  public void whenLengthInvalidRange()
    {
    // Given...
    DateDomain domain = new DateDomain();

    expectFailure( ValueDomainException.class)
      .when( () -> domain.setLengthRange( Range.of( VarBindingBuilder.with( "Length").value( "> 0").build())));
    }

  @Test
  public void whenLengthInvalidConstant()
    {
    // Given...
    DateDomain domain = new DateDomain();

    expectFailure( ValueDomainException.class)
      .when( () -> domain.setLengthRange( 8));
    }
  }
