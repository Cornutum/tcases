//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.resolve.DataValue.Type;
import static org.cornutum.tcases.resolve.DataValue.Type.*;
import static org.cornutum.tcases.resolve.DataValues.*;

import java.math.BigDecimal;

/**
 * Builds {@link Schema} instances.
 */
public class SchemaBuilder
  {
  /**
   * Creates a new SchemaBuilder instance.
   */
  private SchemaBuilder( Schema schema)
    {
    schema_ = schema;
    }

  /**
   * Returns a builder for a schema of the given type.
   */
  public static SchemaBuilder type( String type)
    {
    return type( Type.valueOf( type.toUpperCase()));
    }

  /**
   * Returns a builder for a schema of the given type.
   */
  public static SchemaBuilder type( Type type)
    {
    return new SchemaBuilder( new Schema( type));
    }

  /**
   * Returns a builder for a schema of undefined type.
   */
  public static SchemaBuilder generic()
    {
    return type( NULL);
    }

  public SchemaBuilder constant( DataValue<?> constant)
    {
    schema_.setConstant( constant);
    return this;
    }

  @SuppressWarnings("unchecked")
  public <T> SchemaBuilder constant( DataValue<T>... items)
    {
    schema_.setConstant( arrayOf( items));
    return this;
    }

  public SchemaBuilder constant( Boolean value)
    {
    schema_.setConstant( valueOf( value));
    return this;
    }

  public SchemaBuilder constant( BigDecimal value)
    {
    schema_.setConstant( valueOf( value));
    return this;
    }

  public SchemaBuilder constant( int value)
    {
    schema_.setConstant( valueOf( value));
    return this;
    }

  public SchemaBuilder constant( long value)
    {
    schema_.setConstant( valueOf( value));
    return this;
    }

  public SchemaBuilder constant( String value)
    {
    schema_.setConstant( stringOf( value));
    return this;
    }

  public SchemaBuilder constant( String format, String value)
    {
    return constant( stringOf( format, value)).format( format);
    }
  
  public SchemaBuilder format( String format)
    {
    schema_.setFormat( format);
    return this;
    }
  
  public SchemaBuilder minimum( BigDecimal minimum)
    {
    schema_.setMinimum( minimum);
    return this;
    }
  
  public SchemaBuilder maximum( BigDecimal maximum)
    {
    schema_.setMaximum( maximum);
    return this;
    }
  
  public SchemaBuilder exclusiveMinimum( BigDecimal exclusiveMinimum)
    {
    schema_.setExclusiveMinimum( exclusiveMinimum);
    return this;
    }
  
  public SchemaBuilder exclusiveMaximum( BigDecimal exclusiveMaximum)
    {
    schema_.setExclusiveMaximum( exclusiveMaximum);
    return this;
    }
  
  public SchemaBuilder multipleOf( BigDecimal multipleOf)
    {
    schema_.setMultipleOf( multipleOf);
    return this;
    }
  
  public SchemaBuilder minLength( Integer minLength)
    {
    schema_.setMinLength( minLength);
    return this;
    }
  
  public SchemaBuilder maxLength( Integer maxLength)
    {
    schema_.setMaxLength( maxLength);
    return this;
    }
  
  public SchemaBuilder pattern( String pattern)
    {
    schema_.setPattern( pattern);
    return this;
    }
  
  public SchemaBuilder minItems( Integer minItems)
    {
    schema_.setMinItems( minItems);
    return this;
    }
  
  public SchemaBuilder maxItems( Integer maxItems)
    {
    schema_.setMaxItems( maxItems);
    return this;
    }
  
  public SchemaBuilder uniqueItems( Boolean uniqueItems)
    {
    schema_.setUniqueItems( uniqueItems);
    return this;
    }
  
  public SchemaBuilder items( Schema items)
    {
    schema_.setItems( items);
    return this;
    }
  
  public Schema build()
    {
    return schema_;
    }  

    private Schema schema_;
  }
