//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator;

import com.startingblocktech.tcases.*;
import com.startingblocktech.tcases.util.ToString;

import org.apache.commons.lang.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
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
   * Represents a pattern matching one or more input variable names.
   *
   * @version $Revision$, $Date$
   */
  private static class VarNamePattern
    {
    /**
     * Creates a new VarNamePattern object.
     */
    public VarNamePattern( String pattern)
      {
      varNamePath_ =
        StringUtils.isBlank( pattern)
        ? null
        : pattern.split( "\\.");

      int patternLength = varNamePath_==null? 0 : varNamePath_.length;
      for( minDepth_ = 0,
             wildcard_ = false;

           minDepth_ < patternLength
             && !(wildcard_ = ALL_DESCENDANTS.equals( varNamePath_[ minDepth_]) || ALL_CHILDREN.equals( varNamePath_[ minDepth_]));
           
           minDepth_++);
      }

    /**
     * Returns true if the given variable name matches this pattern.
     */
    public boolean matches( VarNamePattern varName)
      {
      boolean matched =
        // Variable name defined?
        varName != null

        // Variable name and this pattern both (non-)empty?
        && (varName.varNamePath_ == null) == (varNamePath_ == null)

        // Variable name identifies single variable of depth no less than this pattern?
        && !varName.wildcard_
        && varName.minDepth_ >= minDepth_;

      if( matched && varNamePath_ != null)
        {
        // Variable name matches this pattern up to wildcard?
        int i;
        for( i = 0;

             i < minDepth_
               && (matched = varName.varNamePath_[i].equals( varNamePath_[i]));

             i++);

        if( matched)
          {
          int membersLeft = varName.minDepth_ - minDepth_;

          String wildcardType =
            !wildcard_
            ? null
            : varNamePath_[i];
          
          matched =
            // No wildcard to match?
            wildcardType == null
            ? (membersLeft == 0)

            // This variable is a matching child?
            : ((membersLeft == 1 && wildcardType.equals( ALL_CHILDREN))
               ||
               // This variable is a matching descendant?
               wildcardType.equals( ALL_DESCENDANTS));
          }
        }
      
      return matched;
      }

    /**
     * Returns true if this pattern is valid.
     */
    public boolean isValid()
      {
      // Must be non-empty...
      boolean valid = varNamePath_ != null;

      if( valid)
        {
        // Containing a sequence of identifiers...
        int i;
        for( i = 0; i < varNamePath_.length && DefUtils.isIdentifier( varNamePath_[i]); i++);

        // Optionally terminated by a single wildcard.
        int membersLeft = varNamePath_.length - i;
        valid =
          membersLeft == 0
          ||
          (membersLeft == 1
           &&
           (varNamePath_[i].equals( ALL_DESCENDANTS) || varNamePath_[i].equals( ALL_CHILDREN)));
        }

      return valid;
      }

    /**
     * Returns true if this pattern is applicable to the given input definition.
     */
    public boolean isApplicable( FunctionInputDef inputDef)
      {
      return isApplicable( inputDef.getVarDefs());
      }

    /**
     * Returns true if this pattern is applicable to the given set of variables.
     */
    private boolean isApplicable( Iterator<IVarDef> varDefs)
      {
      boolean applicable = false;
      while( varDefs.hasNext() && !(applicable = isApplicable( varDefs.next())));
      return applicable;
      }

    /**
     * Returns true if this pattern is applicable to the given variable.
     */
    private boolean isApplicable( IVarDef var)
      {
      Iterator<IVarDef> members = var.getMembers();

      return
        members == null
        ? matches( new VarNamePattern( var.getPathName()))
        : isApplicable( members);
      }

    public boolean equals( Object object)
      {
      return
        object != null
        && object.getClass().equals( getClass())
        && Arrays.equals( varNamePath_, ((VarNamePattern) object).varNamePath_);
      }

    public int hashCode()
      {
      return
        varNamePath_ == null
        ? 0
        : varNamePath_.hashCode();
      }

    public String toString()
      {
      return StringUtils.join( varNamePath_, '.');
      }
    
    private String[] varNamePath_;
    private int minDepth_;
    private boolean wildcard_;
    
    private static final String ALL_DESCENDANTS = "**";
    private static final String ALL_CHILDREN    = "*";
    }

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
    List<IVarDef> vars = getCombinedVars( inputDef);
    return null;
    }

  /**
   * Returns the set of input variables to be combined.
   */
  private List<IVarDef> getCombinedVars( FunctionInputDef inputDef)
    {
    assertApplicable( inputDef);
    List<IVarDef> combinedVars = addCombinedVars( new ArrayList<IVarDef>(), inputDef.getVarDefs());
    if( combinedVars.size() < getTupleSize())
      {
      throw
        new IllegalStateException
        ( "Can't return " + getTupleSize() + "-tuples for " + inputDef + ": "
          + combinedVars.size() + " variables eligible for combination");
      }

    return combinedVars;
    }

  /**
   * Adds to the <code>combinedVars</code> list all members of the given variable set that are eligible to be combined
   * and returns the <code>combinedVars</code> list.
   */
  private List<IVarDef> addCombinedVars( List<IVarDef> combinedVars, Iterator<IVarDef> varDefs)
    {
    while( varDefs.hasNext())
      {
      IVarDef varDef = varDefs.next();
      Iterator<IVarDef> members = varDef.getMembers();
      if( members == null)
        {
        VarNamePattern varNamePath = new VarNamePattern( varDef.getPathName());
        if( !isExcluded( varNamePath) && isIncluded( varNamePath))
          {
          combinedVars.add( varDef);
          }
        }
      else
        {
        addCombinedVars( combinedVars, members);
        }
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
