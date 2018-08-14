//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2015, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.util;

import static org.cornutum.tcases.util.Asserts.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.junit.Test;

/**
 * Runs tests for {@link CartesianProduct} methods.
 *
 */
public class TestCartesianProduct
  {
  @Test
  public void whenManySets()
    {
    // Given...
    List<Set<String>> sets =
      new ListBuilder<Set<String>>()
      .add( new TreeSet<String>( Arrays.asList( "A", "B", "C")))
      .add( new TreeSet<String>( Arrays.asList( "1", "2", "3", "4")))
      .add( new TreeSet<String>( Arrays.asList( "X")))
      .build();
      
    // When...
    CartesianProduct<String> product = new CartesianProduct<String>( sets);
    
    // Then...
    List<List<String>> expected =
      new ListBuilder<List<String>>()
      .add( new ArrayList<String>( Arrays.asList( "A", "1", "X")))
      .add( new ArrayList<String>( Arrays.asList( "A", "2", "X")))
      .add( new ArrayList<String>( Arrays.asList( "A", "3", "X")))
      .add( new ArrayList<String>( Arrays.asList( "A", "4", "X")))
      .add( new ArrayList<String>( Arrays.asList( "B", "1", "X")))
      .add( new ArrayList<String>( Arrays.asList( "B", "2", "X")))
      .add( new ArrayList<String>( Arrays.asList( "B", "3", "X")))
      .add( new ArrayList<String>( Arrays.asList( "B", "4", "X")))
      .add( new ArrayList<String>( Arrays.asList( "C", "1", "X")))
      .add( new ArrayList<String>( Arrays.asList( "C", "2", "X")))
      .add( new ArrayList<String>( Arrays.asList( "C", "3", "X")))
      .add( new ArrayList<String>( Arrays.asList( "C", "4", "X")))
      .build();
    
    assertSeqEquals( "When many sets", expected, product);
    }

  @Test
  public void whenFiltered()
    {
    // Given...
    List<Set<String>> sets =
      new ListBuilder<Set<String>>()
      .add( new TreeSet<String>( Arrays.asList( "A", "B", "C")))
      .add( new TreeSet<String>( Arrays.asList( "1", "2", "3", "4")))
      .add( new TreeSet<String>( Arrays.asList( "X")))
      .build();
      
    // When...
    CartesianProduct<String> product =
      new CartesianProduct<String>(
        sets,
        candidate -> !(candidate.contains( "3") || candidate.contains( "B")));
    
    // Then...
    List<List<String>> expected =
      new ListBuilder<List<String>>()
      .add( new ArrayList<String>( Arrays.asList( "A", "1", "X")))
      .add( new ArrayList<String>( Arrays.asList( "A", "2", "X")))
      .add( new ArrayList<String>( Arrays.asList( "A", "4", "X")))
      .add( new ArrayList<String>( Arrays.asList( "C", "1", "X")))
      .add( new ArrayList<String>( Arrays.asList( "C", "2", "X")))
      .add( new ArrayList<String>( Arrays.asList( "C", "4", "X")))
      .build();
    
    assertSeqEquals( "When filtered", expected, product);
      
    // When...
    product =
      new CartesianProduct<String>(
        sets,
        candidate -> !candidate.contains( "X"));
    
    // Then...
    expected = Collections.<List<String>>emptyList();
    assertSeqEquals( "When filtered to empty", expected, product);
    }


  @Test
  public void whenEmptySet()
    {
    // Given...
    List<Set<String>> sets =
      new ListBuilder<Set<String>>()
      .add( new TreeSet<String>( Arrays.asList( "A", "B", "C")))
      .add( new TreeSet<String>())
      .add( new TreeSet<String>( Arrays.asList( "X")))
      .build();
      
    // When...
    CartesianProduct<String> product = new CartesianProduct<String>( sets);
    
    // Then...
    List<List<String>> expected = Collections.<List<String>>emptyList();
    
    assertSeqEquals( "When empty set", expected, product);
    }


  @Test
  public void whenOneSet()
    {
    // Given...
    List<Set<String>> sets =
      new ListBuilder<Set<String>>()
      .add( new TreeSet<String>( Arrays.asList( "A", "B", "C")))
      .build();
      
    // When...
    CartesianProduct<String> product = new CartesianProduct<String>( sets);
    
    // Then...
    List<List<String>> expected = 
      new ListBuilder<List<String>>()
      .add( new ArrayList<String>( Arrays.asList( "A")))
      .add( new ArrayList<String>( Arrays.asList( "B")))
      .add( new ArrayList<String>( Arrays.asList( "C")))
      .build();
    
    assertSeqEquals( "When one set", expected, product);
    }


  @Test
  public void whenNoSets()
    {
    // Given...
    List<Set<String>> sets = Collections.<Set<String>>emptyList();
      
    // When...
    CartesianProduct<String> product = new CartesianProduct<String>( sets);
    
    // Then...
    List<List<String>> expected = Collections.<List<String>>emptyList();
    
    assertSeqEquals( "When no sets", expected, product);
    }

  private class ListBuilder<T>
    {
    public ListBuilder<T> add( T member)
      {
      list_.add( member);
      return this;
      }

    public List<T> build()
      {
      return list_;
      }

    private List<T> list_ = new ArrayList<T>();
    }
  }

