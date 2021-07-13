//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.conditions.ICondition;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Builds {@link VarSet} instances.
 *
 */
public class VarSetBuilder extends AnnotatedBuilder<VarSetBuilder>
  {
  /**
   * Creates a new builder for a VarSet with the given name.
   */
  public static VarSetBuilder with( String name)
    {
    return new VarSetBuilder().name( name);
    }
  
  /**
   * Creates a new builder for the given VarSet.
   */
  public static VarSetBuilder with( VarSet varSet)
    {
    return new VarSetBuilder( varSet);
    }


  /**
   * Creates a new VarSetBuilder object.
   */
  public VarSetBuilder()
    {
    this( null);
    }
  
  /**
   * Creates a new VarSetBuilder object.
   */
  public VarSetBuilder( VarSet varSet)
    {
    start( varSet);
    }

  /**
   * Returns the current VarSet definition.
   */
  public VarSet build()
    {
    return varSet_;
    }

  /**
   * Starts building a new VarSet definition.
   */
  public VarSetBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a VarSet definition.
   */
  public VarSetBuilder start( VarSet varSet)
    {
    varSet_ =
      varSet == null
      ? new VarSet( "V")
      : varSet;
    
    return this;
    }

  /**
   * Changes the variable set name.
   */
  public VarSetBuilder name( String name)
    {
    varSet_.setName( name);
    return this;
    }

  /**
   * Changes the variable set type.
   */
  public VarSetBuilder type( String type)
    {
    varSet_.setType( type);
    return this;
    }

  /**
   * Changes the variable set condition.
   */
  public VarSetBuilder when( ICondition condition)
    {
    varSet_.setCondition( condition);
    return this;
    }

  /**
   * Changes the variable set condition.
   */
  public VarSetBuilder when( Optional<ICondition> condition)
    {
    condition.ifPresent( c -> when( c));
    return this;
    }

  /**
   * Adds variable set members.
   */
  public VarSetBuilder members( IVarDef... members)
    {
    for( IVarDef member : members)
      {
      varSet_.addMember( member);
      }
    return this;
    }

  /**
   * Adds variable set members.
   */
  public VarSetBuilder members( Iterable<IVarDef> members)
    {
    for( IVarDef member : members)
      {
      varSet_.addMember( member);
      }
    return this;
    }

  /**
   * Adds variable set members.
   */
  public VarSetBuilder members( Stream<IVarDef> members)
    {
    members.forEach( member -> varSet_.addMember( member));
    return this;
    }

  /**
   * Adds a new {@link VarSet} with the given path name and returns a builder
   * for the new <CODE>VarSet</CODE>.
   */
  public VarSetBuilder varSetAtPath( String pathName)
    {
    return varSetAtPath( DefUtils.toPath( pathName));
    }

  /**
   * Adds a new {@link VarSet} with the given path name and returns a builder
   * for the new <CODE>VarSet</CODE>.
   */
  public VarSetBuilder varSetAtPath( String[] path)
    {
    VarSet varSet = null;
    if( path != null && path.length > 0)
      {
      varSet = new VarSet( path[0]);
      varSet_.addMember( varSet);

      for( int i = 1; i < path.length; i++)
        {
        VarSet child = new VarSet( path[i]);
        varSet.addMember( child);
        varSet = child;
        }
      }

    return
      varSet == null
      ? null
      : new VarSetBuilder( varSet);
    }

  /**
   * Adds a new {@link VarDef} with the given path name and returns a builder
   * for the new <CODE>VarDef</CODE>.
   */
  public VarDefBuilder varDefAtPath( String pathName)
    {
    return varDefAtPath( DefUtils.toPath( pathName));
    }

  /**
   * Adds a new {@link VarDef} with the given path name and returns a builder
   * for the new <CODE>VarDef</CODE>.
   */
  public VarDefBuilder varDefAtPath( String[] path)
    {
    VarDefBuilder varDefBuilder = null;
    if( path != null && path.length > 0)
      {
      String varDefName = path[ path.length - 1];
      VarSetBuilder parentBuilder = varSetAtPath( Arrays.copyOfRange( path, 0, path.length - 1));
      if( parentBuilder != null)
        {
        varDefBuilder = parentBuilder.varDefAtPath( varDefName);
        }
      else
        {
        VarDef varDef = new VarDef( varDefName);
        varSet_.addMember( varDef);
        varDefBuilder = new VarDefBuilder( varDef);
        }
      }

    return varDefBuilder;
    }

  /**
   * Returns the {@link Annotated} instance for this builder.
   */
  @Override
protected Annotated getAnnotated()
    {
    return varSet_;
    }

  VarSet varSet_;
  }
