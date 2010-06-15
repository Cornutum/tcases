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
   * Removes all pattern matching input variables to be included in this combination.
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
   * Returns all valid N-tuples of values for the included input variables.
   */
  public Iterator<VarBinding[]> getTuples( FunctionInputDef inputDef)
    {
    List<VarDef> vars = getCombinedVars( inputDef);
    return null;
    }

  /**
   * Returns the set of input variables to be combined.
   */
  private List<VarDef> getCombinedVars( FunctionInputDef inputDef)
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

  private int tupleSize_;
  private Set<VarNamePattern> includedVars_;
  private Set<VarNamePattern> excludedVars_;
  }
