//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.Characters;

import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
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
    String value = "me;myself;i@mydomain.org";
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

    // Given...
    String trailingAt = "me@mydomain.org@";

    expectFailure( ValueDomainException.class)
      .when( () -> new EmailConstant( trailingAt));

    // Given...
    String valueNotAllowed = "me;myself;i@mydomain.org";

    expectFailure( ValueDomainException.class)
      .when( () -> new EmailConstant( valueNotAllowed, Characters.COOKIE_VALUE));

    // Given...
    String notToken = "me@mydomain.org";

    expectFailure( ValueDomainException.class)
      .when( () -> new EmailConstant( notToken, Characters.TOKEN));
    }

  @Test
  public void whenEnum()
    {
    // Given...
    List<String> emails = Arrays.asList( "me@mydomain.org", "me@mydomain.org", "myself@mydomain.org");
    EmailEnum domain = new EmailEnum( emails);

    // Then...
    List<String> values = valuesOf( domain, 1000);
    assertThat( "Enum values size", values.size(), is( 1000));
    assertThat( "Enum values content", values.stream().collect( toSet()), containsMembers( Arrays.asList( "me@mydomain.org", "myself@mydomain.org")));
    assertThat( "Contains", domain.contains( "myself@mydomain.org"), is( true));
    assertThat( "Contains", domain.contains( "you@mydomain.org"), is( false));
    }

  @Test
  public void whenInvalidEnum()
    {
    // Given...
    List<String> emails = Arrays.asList( "myself");

    expectFailure( ValueDomainException.class)
      .when( () -> new EmailEnum( emails));

    // Given...
    List<String> valueNotAllowed = Arrays.asList( "me;myself;i@mydomain.org");

    expectFailure( ValueDomainException.class)
      .when( () -> new EmailEnum( valueNotAllowed, Characters.COOKIE_VALUE));

    // Given...
    List<String> notToken = Arrays.asList( "me@mydomain.org");

    expectFailure( ValueDomainException.class)
      .when( () -> new EmailEnum( notToken, Characters.TOKEN));
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
    domain.setMatching( ".org$");
    domain.setNotMatching( "^\\d");

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( "you@mydomain.org"), is( true));
    assertThat( "Contains", domain.contains( "myself@mydomain.org"), is( false));
    assertThat( "Contains", domain.contains( "you@mydomain.com"), is( false));
    assertThat( "Contains", domain.contains( "9ou@mydomain.org"), is( false));

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
    domain.setLengthRange( 0, 6);

    // When...
    DataValue<String> value = domain.select( getResolverContext());

    // Then...
    assertThat( "Invalid email", EmailDomain.isEmail( value.getValue()), is( false));
    }

  @Test
  public void whenLengthInvalidConstant()
    {
    // Given...
    EmailDomain domain = new EmailDomain();
    domain.setLengthRange( 6);

    // When...
    DataValue<String> value = domain.select( getResolverContext());

    // Then...
    assertThat( "Invalid email", EmailDomain.isEmail( value.getValue()), is( false));

    // Given...
    domain.setLengthRange( 400);

    // When...
    value = domain.select( getResolverContext());

    // Then...
    assertThat( "Invalid email", EmailDomain.isEmail( value.getValue()), is( false));
    }

  @Test
  public void whenCharsDefined()
    {
    // Given...
    EmailDomain domain = new EmailDomain( Characters.COOKIE_VALUE);

    // Then
    assertThat( "Email cookie", domain.contains( "me;myself;i@mydomain.org"), is( false));

    List<String> values = valuesOf( domain, 1000);
    assertThat(
      "Email cookies",
      values.stream().filter( e -> !Characters.COOKIE_VALUE.allowed( e)).findFirst(),
      is( Optional.empty()));
    }
  }
