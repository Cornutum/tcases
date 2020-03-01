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
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.stream.Stream;
import static java.util.stream.Collectors.toSet;

import java.util.List;

/**
 * Runs tests for {@link IntegerDomain}.
 */
public class IntegerDomainTest extends ValueDomainTest
  {
  @Test
  public void whenNotMultipleOf()
    {
    // Given...
    IntegerDomain domain = new IntegerDomain();

    // When...
    domain.setRange( Range.of( VarBindingBuilder.with( "Integer").value( "> 100").build()));
    domain.setNotMultipleOfs( new String[]{ "2" });

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( 101), is( true));
    assertThat( "Contains", domain.contains(  99), is( false));
    assertThat( "Contains", domain.contains( 102), is( false));
    }

  @Test
  public void whenConstant()
    {
    // Given...
    IntegerConstant domain = new IntegerConstant( -99);

    // Then...
    List<Integer> values = valuesOf( domain, 10);
    assertThat( "Constant values size", values.size(), is( 1));
    assertThat( "Constant value", domain.select( getResolverContext()), matches( dataValueMatcher( -99, Type.INTEGER, "int32")));
    }

  @Test
  public void whenMultipleOf()
    {
    // Given...
    IntegerDomain domain = new IntegerDomain();

    // When...
    domain.setRange( Range.of( VarBindingBuilder.with( "Integer").value( "< 10").build()));
    domain.setMultipleOf( 5);

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( -5), is( true));
    assertThat( "Contains", domain.contains( 10), is( false));
    assertThat( "Contains", domain.contains( -4), is( false));
    }

  @Test
  public void whenNotMultipleOfs()
    {
    // Given...
    IntegerDomain domain = new IntegerDomain();

    // When...
    domain.setRange( 4, 20);
    domain.setMultipleOf( "3");
    domain.setNotMultipleOfs( Stream.of( 5, 2).collect( toSet()));

    // Then...
    List<Integer> values = valuesOf( domain, 10);
    assertThat( "Values size", values.size(), is( 1));
    assertThat( "Value", domain.select( getResolverContext()), matches( dataValueMatcher( 9, Type.INTEGER, "int32")));

    // When...
    domain.setRange( 13, 20);
    
    // Then...
    values = valuesOf( domain, 10);
    assertThat( "Values size", values.size(), is( 0));
    expectFailure( IllegalStateException.class).when( () -> domain.select( getResolverContext()));

    // When...
    domain.setRange( -20, 20);
    
    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains(  -9), is( true));
    assertThat( "Contains", domain.contains( -21), is( false));
    assertThat( "Contains", domain.contains(  18), is( false));
    assertThat( "Contains", domain.contains( -15), is( false));
    }

  @Test
  public void whenRangeTooBig()
    {
    // Given...
    IntegerDomain domain = new IntegerDomain(); 

    // Then...
    expectFailure( IllegalArgumentException.class)
      .when( () -> domain.setRange( Integer.MIN_VALUE, Integer.MAX_VALUE));
    }

  @Test
  public void whenExcluded()
    {
    // Given...
    IntegerDomain domain = new IntegerDomain();

    // When...
    domain.setRange(
      Range.of(
        VarBindingBuilder.with( "Integer")
        .value( "Other")
        .has( "excluded", -11, 1, 11).build()));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains(   2), is( true));
    assertThat( "Contains", domain.contains(   1), is( false));
    assertThat( "Contains", domain.contains( 111), is( true));
    }
  }
