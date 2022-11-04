//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.NumberDomain.Range;
import org.cornutum.tcases.resolve.BooleanValue;
import org.cornutum.tcases.resolve.DataValue;
import org.cornutum.tcases.resolve.IntegerValue;
import org.cornutum.tcases.resolve.LongValue;
import org.cornutum.tcases.resolve.StringValue;
import org.junit.Test;

import static org.cornutum.tcases.resolve.DataValue.Type;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.Arrays;
import java.util.List;
import static java.util.stream.Collectors.toList;

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

    assertThat( "Contains", domain.contains( listOf( new String[0])), is( true));
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
    ArrayDomain<Object> domain = new ArrayDomain<Object>(4);

    // When...
    domain.setItemCount( new IntegerConstant( 4));
    domain.setItemValues( new MultiTypeDomain( Type.BOOLEAN, Type.STRING, Type.INTEGER));
    domain.setItemsUnique( true);
    
    // Then...
    verifyContainsValues( domain, 1000);

    assertThat(
      "Contains",
      domain.contains(
        listOf(
          new BooleanValue(true),
          new StringValue("true"),
          new IntegerValue(0),
          new IntegerValue(1))),
      is( true));
    assertThat(
      "Contains",
      domain.contains(
        listOf(
          new StringValue("one"),
          new StringValue("two"),
          new StringValue("three"),
          new StringValue("four"))),
      is( true));
    assertThat(
      "Contains",
      domain.contains(
        listOf(
          new StringValue("one"),
          new IntegerValue(2),
          new StringValue("three"),
          new IntegerValue(2))),
      is( false));
    assertThat(
      "Contains",
      domain.contains(
        listOf(
          new StringValue("one"),
          new IntegerValue(2),
          new StringValue("three"))),
      is( false));
    assertThat(
      "Contains",
      domain.contains(
        listOf(
          new BooleanValue(true),
          new StringValue("true"),
          new IntegerValue(0),
          new BooleanValue(false),
          new IntegerValue(1))),
      is( false));
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

    assertThat( "Contains", domain.contains( listOf( new String[0])), is( true));
    assertThat( "Contains", domain.contains( listOf( "Oops")), is( false));
    }

  /**
   * Returns a list of the given items.
   */
  @SafeVarargs
  private final List<DataValue<Integer>> listOf( Integer... items)
    {
    return
      Arrays.stream( items)
      .map( IntegerValue::new)
      .collect( toList());
    }

  /**
   * Returns a list of the given items.
   */
  @SafeVarargs
  private final List<DataValue<Long>> listOf( Long... items)
    {
    return
      Arrays.stream( items)
      .map( LongValue::new)
      .collect( toList());
    }

  /**
   * Returns a list of the given items.
   */
  @SafeVarargs
  private final List<DataValue<String>> listOf( String... items)
    {
    return
      Arrays.stream( items)
      .map( StringValue::new)
      .collect( toList());
    }

  /**
   * Returns a list of the given items.
   */
  @SuppressWarnings("unchecked")
  @SafeVarargs
  private final List<DataValue<Object>> listOf( DataValue<?>... items)
    {
    return
      Arrays.stream( items)
      .map( item -> (DataValue<Object>) item)
      .collect( toList());
    }
  }
