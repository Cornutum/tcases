//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.Cloneable;
import org.cornutum.tcases.util.ToString;

import org.apache.commons.collections4.IteratorUtils;
import org.apache.commons.collections4.Transformer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * Generates a combination of values for a specific set of input variables.
 *
 */
public class TupleCombiner implements Cloneable<TupleCombiner>
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
    tupleSize_ = tupleSize;
    setIncludedVars( new HashSet<VarNamePattern>());
    setExcludedVars( new HashSet<VarNamePattern>());
    setOnceTuples( new HashSet<TupleRef>());
    }

  /**
   * Changes the tuple size for input variable combinations. A non-positive tupleSize specifies
   * all permutations.
   */
  public void setTupleSize( int tupleSize)
    {
    Integer onceSize =
      onceTuples_.isEmpty()
      ? null
      : getOnceTuples().next().size();

    if( !(onceSize == null || onceSize == tupleSize))
      {
      throw new IllegalArgumentException( "Tuple size=" + tupleSize + " is not compatible with existing once-only tuple size=" + onceSize);
      }
    
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
   * Returns the set of input variables to be included in this combination.
   */
  public String[] getIncluded()
    {
    return
      IteratorUtils.toArray
      ( IteratorUtils.transformedIterator
        ( includedVars_.iterator(),
          new Transformer<VarNamePattern,String>()
            {
            public String transform( VarNamePattern pattern)
              {
              return pattern.toString();
              }
            }),
        String.class);
    }

  /**
   * Adds a pattern matching input variables to be included in this combination.
   */
  public TupleCombiner addIncludedVar( String varNamePattern)
    {
    getIncludedVars().add( getValidVarNamePattern( varNamePattern));
    return this;
    }

  /**
   * Removes a pattern matching input variables to be included in this combination.
   */
  public TupleCombiner removeIncludedVar( String varNamePattern)
    {
    getIncludedVars().remove( new VarNamePattern( varNamePattern));
    return this;
    }

  /**
   * Removes all patterns matching input variables to be included in this combination.
   */
  public TupleCombiner removeAllIncludedVars()
    {
    getIncludedVars().clear();
    return this;
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
   * Returns the set of input variables to be excluded from this combination.
   */
  public String[] getExcluded()
    {
    return
      IteratorUtils.toArray
      ( IteratorUtils.transformedIterator
        ( excludedVars_.iterator(),
          new Transformer<VarNamePattern,String>()
            {
            public String transform( VarNamePattern pattern)
              {
              return pattern.toString();
              }
            }),
        String.class);
    }

  /**
   * Adds a pattern matching input variables to be excluded from this combination.
   */
  public TupleCombiner addExcludedVar( String varNamePattern)
    {
    getExcludedVars().add( getValidVarNamePattern( varNamePattern));
    return this;
    }

  /**
   * Removes a pattern matching input variables to be excluded from this combination.
   */
  public TupleCombiner removeExcludedVar( String varNamePattern)
    {
    getExcludedVars().remove( new VarNamePattern( varNamePattern));
    return this;
    }

  /**
   * Removes all patterns matching input variables to be excluded from this combination.
   */
  public TupleCombiner removeAllExcludedVars()
    {
    getExcludedVars().clear();
    return this;
    }

  /**
   * Changes the set of once-only tuples in this combination.
   */
  private void setOnceTuples( Set<TupleRef> tupleRefs)
    {
    onceTuples_ = tupleRefs;
    }

  /**
   * Returns an iterator for the set of once-only tuples in this combination.
   */
  public Iterator<TupleRef> getOnceTuples()
    {
    return onceTuples_.iterator();
    }

  /**
   * Adds a pattern matching input variables to be included in this combination.
   */
  public TupleCombiner addOnceTuple( TupleRef tupleRef)
    {
    if( tupleRef != null)
      {
      if( tupleRef.size() != getTupleSize())
        {
        throw new IllegalArgumentException( "Once-only tuple=" + tupleRef + " has size=" + tupleRef.size() + ", expected size=" + getTupleSize());
        }
      onceTuples_.add( tupleRef);
      }
    
    return this;
    }

  /**
   * Removes a pattern matching input variables to be included in this combination.
   */
  public TupleCombiner removeOnceTuple( TupleRef tupleRef)
    {
    onceTuples_.remove( tupleRef);
    return this;
    }

  /**
   * Removes all patterns matching input variables to be included in this combination.
   */
  public TupleCombiner removeAllOnceTuples()
    {
    onceTuples_.clear();
    return this;
    }

  /**
   * Returns all valid N-tuples of values for the included input variables.
   */
  public Collection<Tuple> getTuples( FunctionInputDef inputDef)
    {
    List<VarDef> combinedVars = getCombinedVars( inputDef);
    return getCombinedTuples( combinedVars, getTuples( combinedVars, getTupleSize()));
    }

  /**
   * Returns all valid N-tuples of values for the given input variables. A non-positive tupleSize specifies
   * all permutations.
   */
  public static Collection<Tuple> getTuples( List<VarDef> varDefs, int tupleSize)
    {
    if( tupleSize < 1)
      {
      tupleSize = varDefs.size();
      }
    int varEnd = varDefs.size() - tupleSize + 1;
    if( varEnd <= 0)
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
   * Returns all fully-combined N-tuples of values for the included input variables.
   */
  private Collection<Tuple> getCombinedTuples( List<VarDef> combinedVars, Collection<Tuple> tuples)
    {
    // Apply any once-only constraints.
    Set<Tuple> onceTuples = getOnceTupleDefs( combinedVars);
    if( !onceTuples.isEmpty())
      {
      for( Tuple tuple : tuples)
        {
        tuple.setOnce( onceTuples.contains( tuple));
        }
      }
    
    return tuples;
    }

  /**
   * Returns the set of once-only tuple definitions for this combiner.
   */
  private Set<Tuple> getOnceTupleDefs( final List<VarDef> combinedVars)
    {
    try
      {
      return
        new HashSet<Tuple>
        ( IteratorUtils.toList
          ( IteratorUtils.transformedIterator
            ( getOnceTuples(),
              new Transformer<TupleRef,Tuple>()
                {
                public Tuple transform( TupleRef tupleRef)
                  {
                  return toTuple( combinedVars, tupleRef);
                  }
                })));
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Invalid once-only tuple definition", e);
      }
    }

  /**
   * Converts a reference to a tuple of combined variables.
   */
  private Tuple toTuple( List<VarDef> combinedVars, TupleRef tupleRef)
    {
    if( tupleRef.size() != getTupleSize())
      {
      throw new IllegalStateException( String.valueOf( tupleRef) + " does not match combiner tuple size=" + getTupleSize());
      }

    Tuple tuple = new Tuple();
    for( Iterator<VarBinding> bindings = tupleRef.getVarBindings(); bindings.hasNext(); )
      {
      VarBinding binding = bindings.next();
      VarDef var = findVarPath( combinedVars, binding.getVar());
      if( var == null)
        {
        throw new IllegalStateException( "Var=" + binding.getVar() + " is not included in this combination");
        }

      VarValueDef value = var.getValue( binding.getValue());
      if( value == null)
        {
        throw new IllegalStateException( "Value=" + binding.getValue() + " is not defined for var=" + binding.getVar());
        }

      if( !value.isValid())
        {
        throw new IllegalStateException( "Value=" + binding.getValue() + " is a failure value for var=" + binding.getVar());
        }

      tuple.add( new VarBindingDef( var, value));
      }

    return tuple;
    }

  /**
   * Returns the member of the given variable list with the given path.
   */
  private VarDef findVarPath( List<VarDef> combinedVars, String varPath)
    {
    int i;
    for( i = 0;
         i < combinedVars.size()
           && !varPath.equals( combinedVars.get(i).getPathName());
         i++);

    return
      i < combinedVars.size()
      ? combinedVars.get(i)
      : null;
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
    // If no included patterns specified, include all variables.
    boolean included = getIncludedVars().isEmpty();
    if( !included)
      {
      // Otherwise, look for a matching pattern.
      Iterator<VarNamePattern> includedVars = getIncludedVars().iterator();
      while( includedVars.hasNext() && !(included = includedVars.next().matches( varNamePath)));
      }
    
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
  
  /**
   * Returns a copy of this object.
   */
  public TupleCombiner cloneOf()
    {
    TupleCombiner other = new TupleCombiner();
    other.setTupleSize( getTupleSize());
    other.setIncludedVars( new HashSet<VarNamePattern>( getIncludedVars()));
    other.setExcludedVars( new HashSet<VarNamePattern>( getExcludedVars()));
    other.setOnceTuples( new HashSet<TupleRef>( IteratorUtils.toList( getOnceTuples())));
    return other;
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
      && other.getExcludedVars().equals( getExcludedVars())
      && other.onceTuples_.equals( onceTuples_)
      ; 
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ getTupleSize()
      ^ getIncludedVars().hashCode()
      ^ getExcludedVars().hashCode()
      ^ onceTuples_.hashCode();
    }
  
  private int tupleSize_;
  private Set<VarNamePattern> includedVars_;
  private Set<VarNamePattern> excludedVars_;
  private Set<TupleRef> onceTuples_;
  }
