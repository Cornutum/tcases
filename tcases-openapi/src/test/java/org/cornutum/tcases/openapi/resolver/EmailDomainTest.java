//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;

import java.util.stream.Stream;
import static java.util.stream.Collectors.toSet;

/**
 * Runs tests for {@link EmailDomain}.
 */
public class EmailDomainTest extends ValueDomainTest
  {
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
    }

  @Test
  public void whenLengthInvalidRange()
    {
    // Given...
    EmailDomain domain = new EmailDomain();

    expectFailure( RequestCaseException.class)
      .when( () -> domain.setLengthRange( 7, 400));

    expectFailure( RequestCaseException.class)
      .when( () -> domain.setLengthRange( 6, 32));
    }

  @Test
  public void whenLengthInvalidConstant()
    {
    // Given...
    EmailDomain domain = new EmailDomain();

    expectFailure( RequestCaseException.class)
      .when( () -> domain.setLengthRange( 6));

    expectFailure( RequestCaseException.class)
      .when( () -> domain.setLengthRange( 400));
    }
  }
