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

import java.util.Arrays;
import java.util.List;

/**
 * Runs tests for {@link LongDomain}.
 */
public class LongDomainTest extends ValueDomainTest
  {
  @Test
  public void whenEnum()
    {
    // Given...
    List<String> longs = Arrays.asList( "1", "2", "3", "5", "3", "2", "1");
    LongEnum domain = new LongEnum( longs);

    // Then...
    List<Long> values = valuesOf( domain, 1000);
    assertThat( "Enum values size", values.size(), is( 1000));
    assertThat( "Enum values content", values.stream().collect( toSet()), containsMembers( Arrays.asList( 1L, 2L, 3L, 5L)));
    assertThat( "Contains", domain.contains( 5L), is( true));
    assertThat( "Contains", domain.contains( 8L), is( false));
    }

  @Test
  public void whenInvalidEnum()
    {
    // Given...
    List<String> longs = Arrays.asList( "?");

    expectFailure( ValueDomainException.class)
      .when( () -> new LongEnum( longs));
    }
  
  @Test
  public void whenRangeTooBig()
    {
    // Given...
    LongDomain domain = new LongDomain(); 

    // Then...
    expectFailure( IllegalArgumentException.class)
      .when( () -> domain.setRange( -1L, Long.MAX_VALUE));
    }
  
  @Test
  public void whenNotMultipleOf()
    {
    // Given...
    LongDomain domain = new LongDomain();

    // When...
    domain.setRange( 7L, 10L);
    domain.setMultipleOf( "2");
    domain.setNotMultipleOfs( new String[]{ "5" });

    // Then...
    List<Long> values = valuesOf( domain, 10);
    assertThat( "Values size", values.size(), is( 1));
    assertThat( "Value", domain.select( getResolverContext()), matches( dataValueMatcher( 8L, Type.INTEGER, "int64")));

    // When...
    domain.setRange( 9L, 11L);
    
    // Then...
    values = valuesOf( domain, 10);
    assertThat( "Values size", values.size(), is( 0));
    expectFailure( IllegalStateException.class).when( () -> domain.select( getResolverContext()));

    // When...
    domain.setRange( -11L, 11L);

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( 4L), is( true));
    assertThat( "Contains", domain.contains( 12L), is( false));
    assertThat( "Contains", domain.contains( -10L), is( false));
    }
  
  @Test
  public void whenConstant()
    {
    // Given...
    LongConstant domain = new LongConstant( -123456789L);

    // Then...
    List<Long> values = valuesOf( domain, 10);
    assertThat( "Constant values size", values.size(), is( 1));
    assertThat( "Constant value", domain.select( getResolverContext()), matches( dataValueMatcher( -123456789L, Type.INTEGER, "int64")));
    }
  
  @Test
  public void whenNotMultipleOfs()
    {
    // Given...
    LongDomain domain = new LongDomain();

    // When...
    domain.setRange( Range.of( VarBindingBuilder.with( "Long").value( "< -999999999").build()));
    domain.setNotMultipleOfs( Stream.of( 5L, 2L).collect( toSet()));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( -1000000007L), is( true));
    assertThat( "Contains", domain.contains( -999999999L), is( false));
    assertThat( "Contains", domain.contains( -1000000000L), is( false));
    }
  
  @Test
  public void whenMultipleOf()
    {
    // Given...
    LongDomain domain = new LongDomain();

    // When...
    domain.setRange( Range.of( VarBindingBuilder.with( "Long").value( "> 999999999").build()));
    domain.setMultipleOf( 1000L);

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( 1000000000L), is( true));
    assertThat( "Contains", domain.contains( 1000000999L), is( false));
    assertThat( "Contains", domain.contains( 999999000L), is( false));
    }

  @Test
  public void whenExcluded()
    {
    // Given...
    LongDomain domain = new LongDomain();

    // When...
    domain.setRange(
      Range.of(
        VarBindingBuilder.with( "Long")
        .value( "Other")
        .has( "excluded", 2L, 3L, 5L, 8L, 13L, 21L).build()));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( -2345L), is( true));
    assertThat( "Contains", domain.contains( 21L), is( false));
    assertThat( "Contains", domain.contains( 4L), is( true));
    }
  }
