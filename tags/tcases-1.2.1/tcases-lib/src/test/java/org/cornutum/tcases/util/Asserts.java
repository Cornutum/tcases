//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import static org.junit.Assert.*;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * Defines test assertion utilities.
 *
 * @version $Revision$, $Date$
 */
public abstract class Asserts
  {
  /**
   * Reports a failure if the actual Collection contains a different set of values than the expected Collection.
   */
  public static <T> void assertSetEquals( String label, Collection<T> expected, Collection<T> actual)
    {
    if( (expected == null) != (actual == null))
      {
      assertEquals( label, expected, actual);
      }

    else if( expected != null)
      {
      Collection<T> unexpected = CollectionUtils.subtract( actual, expected);
      Collection<T> missing = CollectionUtils.subtract( expected, actual);

      if( !(unexpected.isEmpty() && missing.isEmpty()))
        {
        StringBuilder msg = new StringBuilder();

        msg.append( label);
        if( !missing.isEmpty())
          {
          msg.append( ", missing=").append( '[').append( StringUtils.join( missing, ',')).append( ']');
          }
        if( !unexpected.isEmpty())
          {
          msg.append( ", unexpected=").append( '[').append( StringUtils.join( unexpected, ',')).append( ']');
          }

        fail( msg.toString());
        }
      }
    }
  /**
   * Reports a failure if the actual Collection contains a different set of values than the expected Collection.
   */
  public static <T> void assertSetEquals( String label, Collection<T> expected, Iterator<T> actual)
    {
    List<T> actualList = actual == null ? null : IteratorUtils.toList( actual);
    assertSetEquals( label, expected, actualList);
    }

  /**
   * Reports a failure if the actual Collection contains a different set of values than the expected array.
   */
  public static <T> void assertSetEquals( String label, T[] expected, Collection<T> actual)
    {
    List<T> expectedList = expected == null ? null : Arrays.asList( expected);
    assertSetEquals( label, expectedList, actual);
    }

  /**
   * Reports a failure if the actual collection contains a different set of values than the expected array.
   */
  public static <T> void assertSetEquals( String label, T[] expected, Iterator<T> actual)
    {
    List<T> expectedList = expected == null ? null : Arrays.asList( expected);
    assertSetEquals( label, expectedList, actual);
    }

  /**
   * Reports a failure if the actual Iterator contains a different sequence of values than the expected List.
   */
  public static <T> void assertSeqEquals( String label, List<T> expected, Iterator<T> actual)
    {
    if( (expected == null) !=(actual == null))
      {
      fail( label
            + " expected: "
            + (expected == null ? null : "[" + StringUtils.join( expected, ",") + "]")
            + ", but was: "
            + (actual == null ? null : "[" + StringUtils.join( IteratorUtils.toArray( actual), ",")
               + "]"));
      }

    else if( expected != null)
      {
      StringBuilder labelBuilder = new StringBuilder();
      int i;
      for (i = 0; i < expected.size() && actual.hasNext(); i++)
        {
        assertEquals
          ( labelBuilder.delete( 0, labelBuilder.length())
            .append( label).append( " [").append( i).append( "]").toString(),
            expected.get( i),
            actual.next());
        }

      if( i < expected.size())
        {
        fail
          ( labelBuilder.delete( 0, labelBuilder.length())
            .append( label)
            .append( " too small, starting at index=").append( i)
            .append( ", missing=[").append( StringUtils.join( expected.toArray(), ",", i, expected.size())).append( "]")
            .toString());
        }
      else if( actual.hasNext())
        {
        fail
          ( labelBuilder.delete( 0, labelBuilder.length())
            .append( label)
            .append( " too big, starting at index=").append( i)
            .append( ", unexpected=[").append( StringUtils.join( IteratorUtils.toArray( actual), ",")).append( "]")
            .toString());
        }
      }
    }

  /**
   * Reports a failure if the actual List contains a different sequence of values than the expected List.
   */
  public static <T> void assertSeqEquals( String label, List<T> expected, List<T> actual)
    {
    Iterator<T> actualIterator = actual == null ? null : actual.iterator();
    assertSeqEquals( label, expected, actualIterator);
    }

  /**
   * Reports a failure if the actual Iterator contains a different sequence of values than the expected array.
   */
  public static <T> void assertSeqEquals( String label, T[] expected, Iterator<T> actual)
    {
    List<T> expectedList = expected == null ? null : Arrays.asList( expected);
    assertSeqEquals( label, expectedList, actual);
    }

  /**
   * Reports a failure if the actual List contains a different sequence of values than the expected array.
   */
  public static <T> void assertSeqEquals( String label, T[] expected, List<T> actual)
    {
    List<T> expectedList = expected == null ? null : Arrays.asList( expected);
    assertSeqEquals( label, expectedList, actual);
    }
  }
