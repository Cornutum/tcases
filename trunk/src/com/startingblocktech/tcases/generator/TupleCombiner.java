//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.*;
import com.startingblocktech.tcases.util.ToString;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * Generates a combination of values for a specific set of input variables.
 *
 * @version $Revision$, $Date$
 */
public class TupleCombiner
  {
  /**
   * Creates a new TupleCombiner that specifies 1-tuples of values of all input variables.
   */
  public TupleCombiner()
    {
    this( 1);
    }
  
  /**
   * Creates a new TupleCombiner that specifies N-tuples of values of all input variables.
   */
  public TupleCombiner( int tupleSize)
    {
    setTupleSize( tupleSize);
    setIncludedVars( new HashSet<VarNamePattern>());
    setExcludedVars( new HashSet<VarNamePattern>());
    }

  /**
   * Changes the tuple size for input variable combinations. A non-positive tupleSize specifies
   * all permutations.
   */
  public void setTupleSize( int tupleSize)
    {
    tupleSize_ = tupleSize;
    }

  /**
   * Returns the tuple size for input variable combinations. A non-positive tupleSize specifies
   * all permutations.
   */
  public int getTupleSize()
    {
    return tupleSize_;
    }

  /**
   * Changes the set of input variables to be included in this combination.
   */
  private void setIncludedVars( Set<VarNamePattern> varNamePatterns)
    {
    includedVars_ = varNamePatterns;
    }

  /**
   * Returns the set of input variables to be included in this combination.
   */
  private Set<VarNamePattern> getIncludedVars()
    {
    return includedVars_;
    }

  /**
   * Adds a pattern matching input variables to be included in this combination.
   */
  public void addIncludedVar( String varNamePattern)
    {
    getIncludedVars().add( getValidVarNamePattern( varNamePattern));
    }

  /**
   * Removes a pattern matching input variables to be included in this combination.
   */
  public void removeIncludedVar( String varNamePattern)
    {
    getIncludedVars().remove( new VarNamePattern( varNamePattern));
    }

  /**
   * Removes all patterns matching input variables to be included in this combination.
   */
  public void removeAllIncludedVars()
    {
    getIncludedVars().clear();
    }

  /**
   * Changes the set of input variables to be excluded from this combination.
   */
  private void setExcludedVars( Set<VarNamePattern> varNamePatterns)
    {
    excludedVars_ = varNamePatterns;
    }

  /**
   * Returns the set of input variables to be excluded from this combination.
   */
  private Set<VarNamePattern> getExcludedVars()
    {
    return excludedVars_;
    }

  /**
   * Adds a pattern matching input variables to be excluded from this combination.
   */
  public void addExcludedVar( String varNamePattern)
    {
    getExcludedVars().add( getValidVarNamePattern( varNamePattern));
    }

  /**
   * Removes a pattern matching input variables to be excluded from this combination.
   */
  public void removeExcludedVar( String varNamePattern)
    {
    getExcludedVars().remove( new VarNamePattern( varNamePattern));
    }

  /**
   * Removes all patterns matching input variables to be excluded from this combination.
   */
  public void removeAllExcludedVars()
    {
    getExcludedVars().clear();
    }

  /**
   * Returns true if no included variable patterns have been defined.
   */
  public boolean isEmpty()
    {
    return getIncludedVars().isEmpty();
    }

  /**
   * Returns all valid N-tuples of values for the included input variables.
   */
  public Collection<Tuple> getTuples( FunctionInputDef inputDef)
    {
    return getTuples( getCombinedVars( inputDef), getTupleSize());
    }

  /**
   * Returns all valid 1-tuples of values for the given input variable.
   */
  public static Collection<Tuple> getTuples( VarDef var)
    {
    return getTuples( Arrays.asList( var), 1);
    }

  /**
   * Returns all valid N-tuples of values for the given input variables.
   */
  protected static Collection<Tuple> getTuples( List<VarDef> varDefs, int tupleSize)
    {
    int varEnd = varDefs.size() - tupleSize + 1;
    if( tupleSize < 1 || varEnd <= 0)
      {
      throw new IllegalArgumentException( "Can't create " + tupleSize + "-tuples for " + varDefs.size() + " combined variables");
      }
    return getTuples( varDefs, 0, varEnd, tupleSize);
    }

  /**
   * Returns all valid tuples of values for the given input variables.
   */
  private static Collection<Tuple> getTuples( List<VarDef> varDefs, int varStart, int varEnd, int tupleSize)
    {
    Collection<Tuple> tuples = new ArrayList<Tuple>();

    // For each variable up to the last included...
    for( int i = varStart; i < varEnd; i++)
      {
      // Combine each valid value...
      VarDef                nextVar   = varDefs.get(i);
      Iterator<VarValueDef> values    = nextVar.getValidValues();
      if( !values.hasNext())
        {
        throw new IllegalStateException( "Can't complete tuples -- no valid values defined for var=" + nextVar);
        }

      // With all subtuples from the remaining variables.
      Collection<Tuple> subTuples =
        tupleSize==1
        ? null
        : getTuples( varDefs, i + 1, varEnd + 1, tupleSize - 1);

      // Only one variable to combine?
      if( subTuples == null)
        {
        // Yes, return list of 1-tuples for this variable.
        while( values.hasNext())
          {
          tuples.add( new Tuple( new VarBindingDef( nextVar, values.next())));
          }
        }

      // Any compatible subtuples for remaining variables?
      else if( !subTuples.isEmpty())
        {
        // Yes, add all compatible combinations with values for this variable.
        while( values.hasNext())
          {
          VarBindingDef nextBinding = new VarBindingDef( nextVar, values.next());
          for( Tuple subTuple : subTuples)
            {
            Tuple nextTuple = new Tuple( nextBinding).addAll( subTuple);
            if( nextTuple.isCompatible())
              {
              tuples.add( nextTuple);
              }
            }
          }
        }
      }
    
    return tuples;
    }

  /**
   * Returns the set of input variables to be combined.
   */
  public List<VarDef> getCombinedVars( FunctionInputDef inputDef)
    {
    assertApplicable( inputDef);

    List<VarDef> combinedVars = new ArrayList<VarDef>();
    for( VarDefIterator varDefs = new VarDefIterator( inputDef); varDefs.hasNext(); )
      {
      VarDef varDef = varDefs.next();
      if( isEligible( varDef))
        {
        combinedVars.add( varDef);
        }
      }

    if( combinedVars.size() < getTupleSize())
      {
      throw
        new IllegalStateException
        ( "Can't return " + getTupleSize() + "-tuples for " + inputDef
          + ": only " + combinedVars.size() + " variables eligible for combination");
      }

    return combinedVars;
    }

  /**
   * Returns true if the given variable matches any excluded input variable.
   */
  private boolean isExcluded( VarNamePattern varNamePath)
    {
    boolean excluded = false;
    Iterator<VarNamePattern> excludedVars = getExcludedVars().iterator();
    while( excludedVars.hasNext() && !(excluded = excludedVars.next().matches( varNamePath)));
    return excluded;
    }

  /**
   * Returns true if the given variable matches any included input variable.
   */
  private boolean isIncluded( VarNamePattern varNamePath)
    {
    boolean included = false;
    Iterator<VarNamePattern> includedVars = getIncludedVars().iterator();
    while( includedVars.hasNext() && !(included = includedVars.next().matches( varNamePath)));
    return included;
    }

  /**
   * Throws an exception if any included/excluded variable pattern is not applicable to the given input definition.
   */
  private void assertApplicable( FunctionInputDef inputDef) throws IllegalArgumentException
    {
    try
      {
      for( VarNamePattern varNamePattern : getIncludedVars())
        {
        assertApplicable( inputDef, varNamePattern);
        }
      for( VarNamePattern varNamePattern : getExcludedVars())
        {
        assertApplicable( inputDef, varNamePattern);
        }
      }
    catch( Exception e)
      {
      throw new IllegalArgumentException( "Can't apply " + this + " to " + inputDef, e);
      }
    }

  /**
   * Throws an exception if the given variable pattern is not applicable to the given input definition.
   */
  private void assertApplicable( FunctionInputDef inputDef, VarNamePattern varNamePattern) throws IllegalArgumentException
    {
    if( !varNamePattern.isApplicable( inputDef))
      {
      throw new IllegalArgumentException( "Can't find variable matching pattern=" + varNamePattern);
      }
    }

  /**
   * Returns true if the given input variable is eligible to be combined.
   */
  public boolean isEligible( IVarDef varDef)
    {
    VarNamePattern varNamePath = new VarNamePattern( varDef.getPathName());
    return !isExcluded( varNamePath) && isIncluded( varNamePath);
    }

  /**
   * Throws an exception if the given variable name pattern is not valid. Otherwise, returns
   * the corresponding {@link VarNamePattern} object.
   */
  private VarNamePattern getValidVarNamePattern( String varNamePattern) throws IllegalArgumentException
    {
    VarNamePattern pattern = new VarNamePattern( varNamePattern);
    if( !pattern.isValid())
      {
      throw new IllegalArgumentException( "\"" + pattern + "\" is not a valid variable name pattern");
      }

    return pattern;
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "tuples", getTupleSize())
      .append( "included", getIncludedVars())
      .append( "excluded", getExcludedVars())
      .toString();
    }

  public boolean equals( Object object)
    {
    TupleCombiner other =
      object != null && object.getClass().equals( getClass())
      ? (TupleCombiner) object
      : null;

    return
      other != null
      && other.getTupleSize() == getTupleSize()
      && other.getIncludedVars().equals( getIncludedVars())
      && other.getExcludedVars().equals( getExcludedVars()); 
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ getTupleSize()
      ^ getIncludedVars().hashCode()
      ^ getExcludedVars().hashCode();
    }
  
  private int tupleSize_;
  private Set<VarNamePattern> includedVars_;
  private Set<VarNamePattern> excludedVars_;
  }
