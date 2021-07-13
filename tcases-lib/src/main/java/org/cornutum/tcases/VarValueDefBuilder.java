//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.VarValueDef.Type;
import org.cornutum.tcases.conditions.ICondition;

import java.util.Collection;
import java.util.Optional;

/**
 * Builds {@link VarValueDef} instances.
 *
 */
public class VarValueDefBuilder extends AnnotatedBuilder<VarValueDefBuilder>
  {
  /**
   * Creates a new builder for a VarValueDef with the given name.
   */
  public static VarValueDefBuilder with( Object name)
    {
    return new VarValueDefBuilder().name( name);
    }
  
  /**
   * Creates a new builder for the given VarValueDef.
   */
  public static VarValueDefBuilder with( VarValueDef varValueDef)
    {
    return new VarValueDefBuilder( varValueDef);
    }


  /**
   * Creates a new VarValueDefBuilder object.
   */
  public VarValueDefBuilder()
    {
    this( null);
    }
  
  /**
   * Creates a new VarValueDefBuilder object.
   */
  public VarValueDefBuilder( VarValueDef varValueDef)
    {
    start( varValueDef);
    }

  /**
   * Returns the current VarValueDef definition.
   */
  public VarValueDef build()
    {
    return varValueDef_;
    }

  /**
   * Starts building a new VarValueDef definition.
   */
  public VarValueDefBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a VarValueDef definition.
   */
  public VarValueDefBuilder start( VarValueDef varValueDef)
    {
    varValueDef_ =
      varValueDef == null
      ? new VarValueDef( "?")
      : varValueDef;
    
    return this;
    }

  /**
   * Changes the value name.
   */
  public VarValueDefBuilder name( Object name)
    {
    varValueDef_.setName( name);
    return this;
    }

  /**
   * Changes the value type.
   */
  public VarValueDefBuilder type( Type type)
    {
    varValueDef_.setType( type);
    return this;
    }

  /**
   * Changes the value condition.
   */
  public VarValueDefBuilder when( ICondition condition)
    {
    varValueDef_.setCondition( condition);
    return this;
    }

  /**
   * Changes the value condition.
   */
  public VarValueDefBuilder when( Optional<ICondition> condition)
    {
    condition.ifPresent( c -> when( c));
    return this;
    }

  /**
   * Adds value properties.
   */
  public VarValueDefBuilder properties( String... properties)
    {
    varValueDef_.addProperties( properties);
    return this;
    }

  /**
   * Adds value properties.
   */
  public VarValueDefBuilder properties( Collection<String> properties)
    {
    varValueDef_.addProperties( properties);
    return this;
    }

  /**
   * Adds a value property, if present.
   */
  public VarValueDefBuilder properties( Optional<String> property)
    {
    property.ifPresent( p -> varValueDef_.addProperties( p));
    return this;
    }

  /**
   * Returns the {@link Annotated} instance for this builder.
   */
  @Override
protected Annotated getAnnotated()
    {
    return varValueDef_;
    }

  VarValueDef varValueDef_;
  }

