//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
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
 * Runs tests for {@link UuidDomain}.
 */
public class UuidDomainTest extends ValueDomainTest
  {  
  @Test
  public void whenConstant()
    {
    // Given...
    String value = "f81d4fae-7dec-11d0-a765-00a0c91e6bf6";
    StringConstant domain = new UuidConstant( value);

    // Then...
    List<String> values = valuesOf( domain, 10);
    assertThat( "Constant values size", values.size(), is( 1));
    assertThat( "Constant value", domain.select( getResolverContext()), matches( dataValueMatcher( value, Type.STRING, "uuid")));
    assertThat( "Contains", domain.contains( value), is( true));
    assertThat( "Contains", domain.contains( ""), is( false));
    }

  @Test
  public void whenInvalidConstant()
    {
    // Given...
    String value = "f81d4fae 7dec 11d0 a765 00a0c91e6bf6";

    expectFailure( ValueDomainException.class)
      .when( () -> new UuidConstant( value));
    }
  
  @Test
  public void whenEnum()
    {
    // Given...
    List<String> uuids = Arrays.asList( "f81d4fae-7dec-11d0-a765-00a0c91e6bf6", "f81d4fae-7dec-11d0-a765-00a0c91e6bf6", "f81d4fae-7dec-11d0-a765-00a0c91e6bf6");
    UuidEnum domain = new UuidEnum( uuids);

    // Then...
    List<String> values = valuesOf( domain, 1000);
    assertThat( "Enum values size", values.size(), is( 1000));
    assertThat( "Enum values content", values.stream().collect( toSet()), containsMembers( Arrays.asList( "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")));
    assertThat( "Contains", domain.contains( "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"), is( true));
    assertThat( "Contains", domain.contains( "f81d4fae-7dec-11d0-a765-00a0c91e6bf7"), is( false));
    }

  @Test
  public void whenInvalidEnum()
    {
    // Given...
    List<String> uuids = Arrays.asList( "H81d4Hae-7dec-11d0-a765-00a0c91e6bH6");

    expectFailure( ValueDomainException.class)
      .when( () -> new UuidEnum( uuids));
    }

  @Test
  public void whenDefaultLength()
    {
    // Given...
    UuidDomain domain = new UuidDomain();

    // When...
    domain.setExcluded( Stream.of( "f81d4fae-7dec-11d0-a765-00a0c91e6bf6").collect( toSet()));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( "F81D4FAE-7DEC-11D0-A765-00A0C91E6BF6"), is( false));
    assertThat( "Contains", domain.contains( ""), is( false));

    // When...
    DataValue<String> value = domain.select( getResolverContext());

    // Then...
    assertThat( "Value", value, matches( dataValueMatcher( value.getValue(), Type.STRING, "uuid")));
    }

  @Test
  public void whenLengthValid()
    {
    // Given...
    UuidDomain domain = new UuidDomain();

    // When...
    domain.setLengthRange(36);
    //   MatchPatterns = Some
    //   NotMatchPatterns = Some

    // Then...
    verifyContainsValues( domain, 1000);
    }

  @Test
  public void whenLengthInvalidRange()
    {
    // Given...
    UuidDomain domain = new UuidDomain();

    expectFailure( ValueDomainException.class)
      .when( () -> domain.setLengthRange( Range.of( VarBindingBuilder.with( "Length").value( "> 0").build())));
    }

  @Test
  public void whenLengthInvalidConstant()
    {
    // Given...
    UuidDomain domain = new UuidDomain();

    expectFailure( ValueDomainException.class)
      .when( () -> domain.setLengthRange( 8));
    }
  }
