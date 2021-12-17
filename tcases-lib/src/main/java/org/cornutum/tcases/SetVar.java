//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Defines a set of values.
 */
public class SetVar extends ListVar
  {
  /**
   * Creates a new SetVar instance.
   */
  public SetVar()
    {
    this( null);
    }
  
  /**
   * Creates a new SetVar instance.
   */
  public SetVar( String name)
    {
    super( name);
    }

  /**
   * Returns true if this variable defines a list of unique values.
   */
  @Override
  public boolean isSet()
    {
    return true;
    }

  /**
   * Changes the key variables that distinguish unique members of this set.
   */
  public void setKey( Iterable<VarNamePattern> varPaths)
    {
    setKey( toStream( varPaths));
    }

  /**
   * Changes the key variables that distinguish unique members of this set.
   */
  public void setKey( String... varNames)
    {
    setKey( Arrays.stream( varNames).map( VarNamePattern::new));
    }

  /**
   * Changes the key variables that distinguish unique members of this set.
   */
  private void setKey( Stream<VarNamePattern> varPaths)
    {
    key_ = new HashSet<VarNamePattern>();
    if( varPaths != null)
      {
      varPaths.forEach( varPath -> addKey( varPath));
      }
    }

  /**
   * Adds a key variable for members of this set.
   */
  public void addKey( VarNamePattern varPath)
    {
    if( !varPath.isValid())
      {
      throw new IllegalArgumentException( String.format( "'%s' is not a valid variable name pattern.", varPath));
      }

    if( !fullPath( varPath).isApplicable( getMemberVarDef()))
      {
      throw new IllegalArgumentException( String.format( "'%s' does not match members of this set.", varPath));
      }

    key_.add( varPath);
    }

  /**
   * Returns the key variables that distinguish unique members of this set.
   */
  public Iterator<VarNamePattern> getKey()
    {
    return key_.iterator();
    }

  /**
   * Returns true if key variables are defined for this set.
   */
  public boolean hasKey()
    {
    return !key_.isEmpty();
    }

  /**
   * Returns the full path to the given set member variable.
   */
  private VarNamePattern fullPath( VarNamePattern memberVarPath)
    {
    return new VarNamePattern( String.format( "%s.%s", getMemberVarDef().getPathName(), memberVarPath));
    }

  private Set<VarNamePattern> key_ = new HashSet<VarNamePattern>();
  }
