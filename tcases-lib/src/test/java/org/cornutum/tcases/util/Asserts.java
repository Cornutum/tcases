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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Defines test assertion utilities.
 *
 */
public abstract class Asserts
  {
  /**
   * Defines a procedure for comparing two objects.
   */
  public interface Matcher<T>
    {
    /**
     * Reports a failure if the expected object does not match the actual object.
     */
    void assertEqual( String label, T expected, T actual);
        
    /**
     * Supports matching when either of the expected or actual values is nullable.
     * Reports a failure if the expected and actual values have different "null-ness".
     * Otherwise, returns true if and only if both values are non-null.
     */
    static <T> boolean matchable( String label, T expected, T actual)
      {
      // Matching "null-ness"?
      if( (expected==null) != (actual==null))
        {
        // No, use assertEquals to report failure.
        assertEquals( label, expected, actual);
        }

      return actual != null;
      }
    }

  /**
   * Reports a failure if the actual collection contains a different set of values than the expected collection.
   */
  public static <T> void assertSetEquals( String label, Iterable<T> expected, Iterable<T> actual)
    {
    assertSetEquals( label, expected, actual, null);
    }

  /**
   * Reports a failure if the actual collection contains a different set of values than the expected collection.
   */
  public static <T> void assertSetEquals( String label, Iterable<T> expected, Iterable<T> actual, Matcher<T> matcher)
    {
    assertSetsEqual( label, toList( expected), toList( actual), matcher);
    }
  
  /**
   * Reports a failure if the actual Collection contains a different set of values than the expected Collection.
   */
  public static <T> void assertSetsEqual( String label, Collection<T> expected, Collection<T> actual)
    {
    assertSetsEqual( label, expected, actual, null);
    }
  
  /**
   * Reports a failure if the actual Collection contains a different set of values than the expected Collection.
   */
  public static <T> void assertSetsEqual( String label, Collection<T> expected, Collection<T> actual, Matcher<T> matcher)
    {
    if( Matcher.matchable( label, expected, actual))
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

      if( matcher != null)
        {
        Map<T,T> actualMembers = mapSelf( actual);
        for( T member : expected)
          {
          assertMatches( label, member, actualMembers.get( member), matcher);
          }
        }
      }
    }
  
  /**
   * Reports a failure if the actual collection contains a different set of values than the expected collection.
   */
  public static <T> void assertSetEquals( String label, Iterable<T> expected, Iterator<T> actual)
    {
    assertSetEquals( label, expected, actual, null);
    }
  
  /**
   * Reports a failure if the actual collection contains a different set of values than the expected collection.
   */
  public static <T> void assertSetEquals( String label, Iterable<T> expected, Iterator<T> actual, Matcher<T> matcher)
    {
    assertSetEquals( label, expected, toList( actual), matcher);
    }

  /**
   * Reports a failure if the actual collection contains a different set of values than the expected array.
   */
  public static <T> void assertSetEquals( String label, T[] expected, Iterable<T> actual)
    {
    assertSetEquals( label, expected, actual, null);
    }

  /**
   * Reports a failure if the actual collection contains a different set of values than the expected array.
   */
  public static <T> void assertSetEquals( String label, T[] expected, Iterable<T> actual, Matcher<T> matcher)
    {
    List<T> expectedList = expected == null ? null : Arrays.asList( expected);
    assertSetEquals( label, expectedList, actual, matcher);
    }

  /**
   * Reports a failure if the actual collection contains a different set of values than the expected array.
   */
  public static <T> void assertSetEquals( String label, T[] expected, Iterator<T> actual)
    {
    assertSetEquals( label, expected, actual, null);
    }

  /**
   * Reports a failure if the actual collection contains a different set of values than the expected array.
   */
  public static <T> void assertSetEquals( String label, T[] expected, Iterator<T> actual, Matcher<T> matcher)
    {
    List<T> expectedList = expected == null ? null : Arrays.asList( expected);
    assertSetEquals( label, expectedList, actual, matcher);
    }

  /**
   * Reports a failure if the actual collection contains a different set of values than the expected collection.
   */
  public static <T> void assertSetEquals( String label, Iterator<T> expected, Iterator<T> actual)
    {
    assertSetEquals( label, expected, actual, null);
    }

  /**
   * Reports a failure if the actual collection contains a different set of values than the expected collection.
   */
  public static <T> void assertSetEquals( String label, Iterator<T> expected, Iterator<T> actual, Matcher<T> matcher)
    {
    assertSetEquals( label, toList( expected), actual, matcher);
    }

  /**
   * Reports a failure if the actual Iterator contains a different sequence of values than the expected List.
   */
  public static <T> void assertSeqEquals( String label, List<T> expected, Iterator<T> actual)
    {
    assertSeqEquals( label, expected, actual, null);
    }
  
  /**
   * Reports a failure if the actual Iterator contains a different sequence of values than the expected List.
   */
  public static <T> void assertSeqEquals( String label, List<T> expected, Iterator<T> actual, Matcher<T> matcher)
    {
    if( (expected == null) != (actual == null))
      {
      fail( label
            + " expected: "
            + (expected == null ? null : "[" + StringUtils.join( expected, ",") + "]")
            + ", but was: "
            + (actual == null ? null : "[" + StringUtils.join( actual, ",") + "]"));
      }

    else if( expected != null)
      {
      StringBuilder labelBuilder = new StringBuilder();
      int i;
      for (i = 0; i < expected.size() && actual.hasNext(); i++)
        {
        assertMatches
          ( labelBuilder.delete( 0, labelBuilder.length())
            .append( label).append( " [").append( i).append( "]").toString(),
            expected.get( i),
            actual.next(),
            matcher);
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
            .append( ", unexpected=[").append( StringUtils.join( actual, ",")).append( "]")
            .toString());
        }
      }
    }

  /**
   * Reports a failure if the actual List contains a different sequence of values than the expected List.
   */
  public static <T> void assertSeqEquals( String label, List<T> expected, List<T> actual)
    {
    assertSeqEquals( label, expected, actual, null);
    }

  /**
   * Reports a failure if the actual List contains a different sequence of values than the expected List.
   */
  public static <T> void assertSeqEquals( String label, List<T> expected, List<T> actual, Matcher<T> matcher)  
    {
    Iterator<T> actualIterator = actual == null ? null : actual.iterator();
    assertSeqEquals( label, expected, actualIterator, matcher);
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

  /**
   * Reports a failure if the actual List contains a different sequence of values than the expected array. Objects are compared
   * using the given Matcher or, if none defined, using equals().
   */
  public static <T> void assertSeqEquals( String label, T[] expected, List<T> actual, Matcher<T> matcher)
    {
    List<T> expectedList = expected == null ? null : Arrays.asList( expected);
    assertSeqEquals( label, expectedList, actual, matcher);
    }

  /**
   * Reports a failure if the actual object does not match the expected object. Objects are compared
   * using the given Matcher or, if none defined, using equals().
   */
  public static <T> void assertMatches( String label, T expected, T actual, Matcher<T> matcher)
    {
    // Matcher defined?
    if( matcher == null)
      {
      // No, compare using equals().
      assertEquals( label, expected, actual);
      }

    else if( Matcher.matchable( label, expected, actual))
      {
      // Use Matcher to compare non-null values.
      matcher.assertEqual( label, expected, actual);
      }
    }

  /**
   * Returns a list containing the given sequence of elements.
   */
  public static <T> List<T> toList( Iterator<T> elements)
    {
    return
      elements == null
      ? null
      : IteratorUtils.toList( elements);
    }

  /**
   * Returns a list containing the given sequence of elements.
   */
  public static <T> List<T> toList( Iterable<T> elements)
    {
    return
      elements == null
      ? null
      : toList( elements.iterator());
    }

  /**
   * Maps each member of the given collection to itself.
   */
  private static <T> Map<T,T> mapSelf( Iterable<T> elements)
    {
    Map<T,T> elementMap = new HashMap<T,T>();
    for( T element : elements)
      {
      elementMap.put( element, element);
      }
    
    return elementMap;
    }
  }
