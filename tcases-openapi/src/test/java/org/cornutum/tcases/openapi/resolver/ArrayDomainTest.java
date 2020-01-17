//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.NumberDomain.Range;
import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Arrays;
import java.util.List;

/**
 * Runs tests for {@link ArrayDomain}.
 */
public class ArrayDomainTest extends ValueDomainTest
  {
  @Test
  public void whenItemsNotUnique()
    {
    // Given...
    ArrayDomain<Integer> domain = new ArrayDomain<Integer>();

    // When...
    domain.setItemCount( Range.of( ">", "3"));
    domain.setItemValues( new IntegerDomain( Range.of( "<", "0")));
    
    // Then...
    verifyContainsValues( domain, 1000);

    assertThat( "Contains", domain.contains( listOf( -1, -23, -456, -1)), is( true));
    assertThat( "Contains", domain.contains( listOf( -1, -23, -456, 0)), is( false));
    assertThat( "Contains", domain.contains( listOf( -1, -1, -1)), is( false));
    }

  @Test
  public void whenItemValuesConstant()
    {
    // Given...
    ArrayDomain<String> domain = new ArrayDomain<String>();

    // When...
    domain.setItemCount( Range.of( "<=", "3"));
    domain.setItemValues( new StringConstant( "Constant"));
    
    // Then...
    verifyContainsValues( domain, 1000);

    assertThat( "Contains", domain.contains( listOf()), is( true));
    assertThat( "Contains", domain.contains( listOf( "Constant", "Constant", "Constant")), is( true));
    assertThat( "Contains", domain.contains( listOf( "Constant", "Constant", "Nope")), is( false));
    assertThat( "Contains", domain.contains( listOf( "Constant", "Constant", "Constant", "Constant")), is( false));
    }

  @Test
  public void whenItemCountConstant()
    {
    // Given...
    ArrayDomain<Long> domain = new ArrayDomain<Long>();

    // When...
    domain.setItemCount( 2);
    domain.setItemValues( new LongDomain( -10L, 10L));
    
    // Then...
    verifyContainsValues( domain, 1000);

    assertThat( "Contains", domain.contains( listOf( 0L, 0L)), is( true));
    assertThat( "Contains", domain.contains( listOf( 10L, 10L)), is( true));
    assertThat( "Contains", domain.contains( listOf( -1L, 0L, 0L)), is( false));
    assertThat( "Contains", domain.contains( listOf( 0L)), is( false));
    assertThat( "Contains", domain.contains( listOf( 0L, -11L)), is( false));
    }

  @Test
  public void whenItemsUnique()
    {
    // Given...
    ArrayDomain<Integer> domain = new ArrayDomain<Integer>(4);

    // When...
    domain.setItemCount( new IntegerConstant( 4));
    domain.setItemValues( new IntegerDomain( 0, 4));
    domain.setItemsUnique( true);
    
    // Then...
    verifyContainsValues( domain, 1000);

    assertThat( "Contains", domain.contains( listOf( 0, 1, 2, 4)), is( true));
    assertThat( "Contains", domain.contains( listOf( 1, 2, 3, 4)), is( true));
    assertThat( "Contains", domain.contains( listOf( 0, 1, 0, 4)), is( false));
    assertThat( "Contains", domain.contains( listOf()), is( false));
    assertThat( "Contains", domain.contains( listOf( 0, 1, 2, 3, 4)), is( false));
    }

  @Test
  public void whenNoItems()
    {
    // Given...
    ArrayDomain<String> domain = new ArrayDomain<String>();

    // When...
    domain.setItemCount( 0);
    
    // Then...
    verifyContainsValues( domain, 1000);

    assertThat( "Contains", domain.contains( listOf()), is( true));
    assertThat( "Contains", domain.contains( listOf( "Oops")), is( false));
    }

  /**
   * Returns a list of the given items.
   */
  @SafeVarargs
  private final <T> List<T> listOf( T... items)
    {
    return Arrays.asList( items);
    }

  }
