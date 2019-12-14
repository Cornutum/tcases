//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import static org.cornutum.hamcrest.Composites.*;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static java.util.stream.Collectors.toList;

/**
 * Runs tests for {@link CollectionUtils} methods
 */
public class CollectionUtilsTest
  {
  @Test
  public void toCsvStrings()
    {
    List<String> values;
    List<String> fromCsv;

    // Given...
    values = Arrays.asList( ",", "10 \\ 2 = 5", "What, me worry?", "'''", "\\\\,", "");

    // When...
    fromCsv =
      CollectionUtils.fromCsv(
        CollectionUtils.toCsv( values.stream()))
      .collect( toList());

    // Then...
    assertThat( "From CSV", fromCsv, listsMembers( values));

    // Given...
    values = Arrays.asList();

    // When...
    fromCsv =
      CollectionUtils.fromCsv(
        CollectionUtils.toCsv( values.stream()))
      .collect( toList());

    // Then...
    assertThat( "From CSV", fromCsv, listsMembers( values));
    }
  
  @Test
  public void toCsvObjects()
    {
    List<Integer> values;
    List<Integer> fromCsv;

    // Given...
    values = Arrays.asList( 1, null, 2345, null);

    // When...
    fromCsv =
      CollectionUtils.fromCsv(
        CollectionUtils.toCsv( values.stream()))
      .map( value -> Optional.ofNullable( value).map( i -> Integer.valueOf(i)).orElse( null))
      .collect( toList());

    // Then...
    assertThat( "From CSV", fromCsv, listsMembers( values));


    // Given...
    values = Arrays.asList( 9);

    // When...
    fromCsv =
      CollectionUtils.fromCsv(
        CollectionUtils.toCsv( values.stream()))
      .map( Integer::valueOf)
      .collect( toList());

    // Then...
    assertThat( "From CSV", fromCsv, listsMembers( values));
    }

  }
