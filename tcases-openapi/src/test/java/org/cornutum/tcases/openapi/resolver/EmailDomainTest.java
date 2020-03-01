//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.List;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toSet;

/**
 * Runs tests for {@link EmailDomain}.
 */
public class EmailDomainTest extends ValueDomainTest
  {
  @Test
  public void whenConstant()
    {
    // Given...
    String value = "me@mydomain.org";
    StringConstant domain = new EmailConstant( value);

    // Then...
    List<String> values = valuesOf( domain, 10);
    assertThat( "Constant values size", values.size(), is( 1));
    assertThat( "Constant value", domain.select( getResolverContext()), matches( dataValueMatcher( value, Type.STRING, "email")));
    assertThat( "Contains", domain.contains( value), is( true));
    assertThat( "Contains", domain.contains( ""), is( false));
    }

  @Test
  public void whenInvalidConstant()
    {
    // Given...
    String value = "1999/01/10";

    expectFailure( ValueDomainException.class)
      .when( () -> new EmailConstant( value));
    }
  
  @Test
  public void whenDefaultLength()
    {
    // Given...
    EmailDomain domain = new EmailDomain();

    // When...
    domain.setExcluded( Stream.of( "me@mydomain.org").collect( toSet()));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( "me@mydomain.org"), is( false));
    assertThat( "Contains", domain.contains( "@"), is( false));
    assertThat( "Contains", domain.contains( "@mydomain.org"), is( false));
    assertThat( "Contains", domain.contains( "MeMyselfAndI"), is( false));
    }

  @Test
  public void whenLengthValid()
    {
    // Given...
    EmailDomain domain = new EmailDomain( 16);

    // When...
    //   MatchPatterns = Some
    //   NotMatchPatterns = Some

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( "you@mydomain.org"), is( true));
    assertThat( "Contains", domain.contains( "myself@mydomain.org"), is( false));

    // When...
    DataValue<String> value = domain.select( getResolverContext());

    // Then...
    assertThat( "Value", value, matches( dataValueMatcher( value.getValue(), Type.STRING, "email")));
    }

  @Test
  public void whenLengthInvalidRange()
    {
    // Given...
    EmailDomain domain = new EmailDomain();

    expectFailure( ValueDomainException.class)
      .when( () -> domain.setLengthRange( 7, 400));

    expectFailure( ValueDomainException.class)
      .when( () -> domain.setLengthRange( 6, 32));
    }

  @Test
  public void whenLengthInvalidConstant()
    {
    // Given...
    EmailDomain domain = new EmailDomain();

    expectFailure( ValueDomainException.class)
      .when( () -> domain.setLengthRange( 6));

    expectFailure( ValueDomainException.class)
      .when( () -> domain.setLengthRange( 400));
    }
  }
