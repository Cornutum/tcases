//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.conditions.ICondition;
import org.cornutum.tcases.resolve.Schema;
import org.cornutum.tcases.resolve.SchemaBuilder;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Builds {@link VarDef} instances.
 *
 */
public class VarDefBuilder extends VarBuilder<VarDefBuilder>
  {
  /**
   * Creates a new builder for a VarDef with the given name.
   */
  public static VarDefBuilder with( String name)
    {
    return new VarDefBuilder().name( name);
    }
  
  /**
   * Creates a new builder for the given VarDef.
   */
  public static VarDefBuilder with( VarDef varDef)
    {
    return new VarDefBuilder( varDef);
    }


  /**
   * Creates a new VarDefBuilder object.
   */
  public VarDefBuilder()
    {
    this( null);
    }
  
  /**
   * Creates a new VarDefBuilder object.
   */
  public VarDefBuilder( VarDef varDef)
    {
    start( varDef);
    }

  /**
   * Returns the current VarDef definition.
   */
  public VarDef build()
    {
    return varDef_;
    }

  /**
   * Starts building a new VarDef definition.
   */
  public VarDefBuilder start()
    {
    return start( null);
    }

  /**
   * Starts building a VarDef definition.
   */
  public VarDefBuilder start( VarDef varDef)
    {
    varDef_ =
      Optional.ofNullable( varDef)
      .map( v ->
            VarDefBuilder.with( v.getName())
            .type( v.getType())
            .when( v.getCondition())
            .schema( SchemaBuilder.with( v.getSchema()).build())
            .values(
              toStream( v.getValues())
              .map( value -> VarValueDefBuilder.with( value).build()))
            .annotations( v)
            .build())
      .orElse( new VarDef( "V"));
    
    return this;
    }

  /**
   * Changes the variable name.
   */
  public VarDefBuilder name( String name)
    {
    varDef_.setName( name);
    return this;
    }

  /**
   * Changes the variable type.
   */
  public VarDefBuilder type( String type)
    {
    varDef_.setType( type);
    return this;
    }

  /**
   * Changes the variable condition.
   */
  public VarDefBuilder when( ICondition condition)
    {
    varDef_.setCondition( condition);
    return this;
    }

  /**
   * Changes the variable condition.
   */
  public VarDefBuilder when( Optional<ICondition> condition)
    {
    condition.ifPresent( c -> when( c));
    return this;
    }

  /**
   * Adds variable values.
   */
  public VarDefBuilder add( VarValueDefBuilder... values)
    {
    return values( Arrays.stream( values).map( VarValueDefBuilder::build));
    }

  /**
   * Adds variable values.
   */
  public VarDefBuilder values( VarValueDef... values)
    {
    for( VarValueDef value : values)
      {
      varDef_.addValue( value);
      }
    return this;
    }

  /**
   * Adds variable values.
   */
  public VarDefBuilder values( Iterable<VarValueDef> values)
    {
    for( VarValueDef value : values)
      {
      varDef_.addValue( value);
      }
    return this;
    }

  /**
   * Adds variable values.
   */
  public VarDefBuilder values( Stream<VarValueDef> values)
    {
    values.forEach( value -> varDef_.addValue( value));
    return this;
    }

  /**
   * Adds a schema.
   */
  public VarDefBuilder set( SchemaBuilder schema)
    {
    return schema( schema.build());
    }

  /**
   * Adds a schema.
   */
  public VarDefBuilder schema( Schema schema)
    {
    varDef_.setSchema( schema);
    return this;
    }

  /**
   * Returns the {@link Annotated} instance for this builder.
   */
  @Override
  protected Annotated getAnnotated()
    {
    return varDef_;
    }

  /**
   * Returns the {@link IVarDef} instance for this builder.
   */
  @Override
  public IVarDef getVarDef()
    {
    return varDef_;
    }

  VarDef varDef_;
  }

