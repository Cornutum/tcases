//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.resolve.DataValue.Type;
import org.cornutum.tcases.util.ToString;
import static org.cornutum.tcases.resolve.DataValues.*;
import static org.cornutum.tcases.resolve.DataValue.Type.*;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toCollection;

/**
 * Defines requirements for the value of a system input variable.
 */
public class Schema
  {  
  /**
   * Creates a new Schema instance of the given type.
   */
  public Schema( Type type)
    {
    setType( assertType( type));
    }
  
  /**
   * Creates a new copy of another Schema instance.
   */
  public Schema( Schema other)
    {
    this( Optional.ofNullable( other).map( Schema::getType).orElse( null));
    setConstant( other.getConstant());
    setEnum( other.getEnum());
    setFormat( other.getFormat());
    setMinimum( other.getMinimum());
    setMaximum( other.getMaximum());
    setExclusiveMinimum( other.getExclusiveMinimum());
    setExclusiveMaximum( other.getExclusiveMaximum());
    setMultipleOf( other.getMultipleOf());
    setMinLength( other.getMinLength());
    setMaxLength( other.getMaxLength());
    setPattern( other.getPattern());
    setMinItems( other.getMinItems());
    setMaxItems( other.getMaxItems());
    setUniqueItems( other.getUniqueItems());
    setItems( Optional.ofNullable( other.getItems()).map( Schema::new).orElse( null));
    }

  /**
   * Creates a new schema by merging the contents of another schema, using this schema as the default.
   */
  public Schema merge( Schema other)
    {
    Schema merged =
      new Schema(
        Optional.of( other.getType())
        .filter( type -> type != NULL)
        .orElse( getType()));

    if( merged.getType() == NULL)
      {
      merged.setFormat( Optional.ofNullable( other.getFormat()).orElse( getFormat()));
      merged.setConstant( other.getConstant() != null? other.getConstant() : getConstant());
      merged.setEnum( other.getEnum() != null? other.getEnum() : getEnum());
      }
    else
      {
      merged.setFormat( Optional.ofNullable( other.getFormat()).orElse( getType() == other.getType()? getFormat() : null));
      merged.setConstant( Optional.ofNullable( other.getConstant()).orElse( null));
      merged.setEnum( Optional.ofNullable( other.getEnum()).orElse( null));

      if( merged.getConstant() == null && merged.getEnum() == null)
        {
        switch( merged.getType())
          {
          case ARRAY:
            {
            merged.setMinItems( Optional.ofNullable( other.getMinItems()).orElse( getMinItems()));
            merged.setMaxItems( Optional.ofNullable( other.getMaxItems()).orElse( getMaxItems()));
            merged.setUniqueItems( Optional.ofNullable( other.getUniqueItems()).orElse( getUniqueItems()));
            merged.setItems( Optional.ofNullable( other.getItems()).orElse( getItems()));
            break;
            }
          case INTEGER:
            {
            merged.setMinimum( roundUp( Optional.ofNullable( other.getMinimum()).orElse( getMinimum())));
            merged.setMaximum( roundDown( Optional.ofNullable( other.getMaximum()).orElse( getMaximum())));
            merged.setExclusiveMinimum( roundDown( Optional.ofNullable( other.getExclusiveMinimum()).orElse( getExclusiveMinimum())));
            merged.setExclusiveMaximum( roundUp( Optional.ofNullable( other.getExclusiveMaximum()).orElse( getExclusiveMaximum())));
            merged.setMultipleOf( Optional.ofNullable( other.getMultipleOf()).orElse( getMultipleOf()));
            break;
            }
          case NUMBER:
            {
            merged.setMinimum( Optional.ofNullable( other.getMinimum()).orElse( getMinimum()));
            merged.setMaximum( Optional.ofNullable( other.getMaximum()).orElse( getMaximum()));
            merged.setExclusiveMinimum( Optional.ofNullable( other.getExclusiveMinimum()).orElse( getExclusiveMinimum()));
            merged.setExclusiveMaximum( Optional.ofNullable( other.getExclusiveMaximum()).orElse( getExclusiveMaximum()));
            merged.setMultipleOf( Optional.ofNullable( other.getMultipleOf()).orElse( getMultipleOf()));
            break;
            }
          case STRING:
            {
            merged.setMinLength( Optional.ofNullable( other.getMinLength()).orElse( getMinLength()));
            merged.setMaxLength( Optional.ofNullable( other.getMaxLength()).orElse( getMaxLength()));
            merged.setPattern( Optional.ofNullable( other.getPattern()).orElse( getPattern()));
            break;
            }
          default:
            {
            break;
            }
          }
        }
      }

    return merged;
    }

  /**
   * Changes the type of input values.
   */
  private void setType( Type type)
    {
    type_ = type;
    }

  /**
   * Returns the type of input values.
   */
  public Type getType()
    {
    return type_;
    }

  /**
   * Changes the constant value of all input values.
   */
  public void setConstant( DataValue<?> constant)
    {
    setType( assertValueType( "const", constant));
    constant_ = constant;
    }

  /**
   * Returns the constant value of all input values.
   */
  public DataValue<?> getConstant()
    {
    return constant_;
    }

  /**
   * Changes the enumeration of all input values.
   */
  public void setEnum( Collection<DataValue<?>> enums)
    {
    setType( assertValueType( "enum", enums));

    Optional.ofNullable( enums)
      .filter( Collection::isEmpty)
      .ifPresent( empty -> {
        throw new IllegalArgumentException( "'enum' must define at least one value");
        });
    
    enums_ = Optional.ofNullable( enums).map( e -> e.stream().collect( toCollection( LinkedHashSet::new))).orElse( null);
    }

  /**
   * Returns the enumeration of all input values.
   */
  public Set<DataValue<?>> getEnum()
    {
    return enums_;
    }

  /**
   * Changes the format of input values.
   */
  public void setFormat( String format)
    {
    format_ = format;
    }

  /**
   * Returns the format of input values.
   */
  public String getFormat()
    {
    return format_;
    }

  /**
   * Changes the minimum (inclusive) for numeric values.
   */
  public void setMinimum( BigDecimal minimum)
    {
    if( minimum != null)
      {
      setType( assertRequiredNumber( "minimum", minimum));
      }
    minimum_ = minimum;
    }

  /**
   * Returns the minimum (inclusive) for numeric values.
   */
  public BigDecimal getMinimum()
    {
    return minimum_;
    }

  /**
   * Changes the maximum (inclusive) for numeric values.
   */
  public void setMaximum( BigDecimal maximum)
    {
    if( maximum != null)
      {
      setType( assertRequiredNumber( "maximum", maximum));
      }
    maximum_ = maximum;
    }

  /**
   * Returns the maximum (inclusive) for numeric values.
   */
  public BigDecimal getMaximum()
    {
    return maximum_;
    }

  /**
   * Changes the minimum (exclusive) for numeric values.
   */
  public void setExclusiveMinimum( BigDecimal exclusiveMinimum)
    {
    if( exclusiveMinimum != null)
      {
      setType( assertRequiredNumber( "exclusiveMinimum", exclusiveMinimum));
      }
    exclusiveMinimum_ = exclusiveMinimum;
    }

  /**
   * Returns the minimum (exclusive) for numeric values.
   */
  public BigDecimal getExclusiveMinimum()
    {
    return exclusiveMinimum_;
    }

  /**
   * Changes the maximum (exclusive) for numeric values.
   */
  public void setExclusiveMaximum( BigDecimal exclusiveMaximum)
    {
    if( exclusiveMaximum != null)
      {
      setType( assertRequiredNumber( "exclusiveMaximum", exclusiveMaximum));
      }
    exclusiveMaximum_ = exclusiveMaximum;
    }

  /**
   * Returns the maximum (exclusive) for numeric values.
   */
  public BigDecimal getExclusiveMaximum()
    {
    return exclusiveMaximum_;
    }

  /**
   * Changes the common divisor for numeric input values.
   */
  public void setMultipleOf( BigDecimal multipleOf)
    {
    if( multipleOf != null)
      {
      setType( assertRequiredNumber( "multipleOf", multipleOf));
      }
    multipleOf_ = multipleOf;
    }

  /**
   * Returns the common divisor for numeric input values.
   */
  public BigDecimal getMultipleOf()
    {
    return multipleOf_;
    }

  /**
   * Changes the minimum length of string values.
   */
  public void setMinLength( Integer minLength)
    {
    if( minLength != null)
      {
      setType( assertRequiredType( "minLength", STRING));
      }
    minLength_ = minLength;
    }

  /**
   * Returns the minimum length of string values.
   */
  public Integer getMinLength()
    {
    return minLength_;
    }

  /**
   * Changes the maximum length of string values.
   */
  public void setMaxLength( Integer maxLength)
    {
    if( maxLength != null)
      {
      setType( assertRequiredType( "maxLength", STRING));
      }
    maxLength_ = maxLength;
    }

  /**
   * Returns the maximum length of string values.
   */
  public Integer getMaxLength()
    {
    return maxLength_;
    }

  /**
   * Changes the regular expression matching string values.
   */
  public void setPattern( String pattern)
    {
    if( pattern != null)
      {
      setType( assertRequiredType( "pattern", STRING));
      }
    pattern_ = pattern;
    }

  /**
   * Returns the regular expression matching string values.
   */
  public String getPattern()
    {
    return pattern_;
    }

  /**
   * Changes the minimum size of array values.
   */
  public void setMinItems( Integer minItems)
    {
    if( minItems != null)
      {
      setType( assertRequiredType( "minItems", ARRAY));
      }
    minItems_ = minItems;
    }

  /**
   * Returns the minimum size of array values.
   */
  public Integer getMinItems()
    {
    return minItems_;
    }

  /**
   * Changes the maximum size of array values.
   */
  public void setMaxItems( Integer maxItems)
    {
    if( maxItems != null)
      {
      setType( assertRequiredType( "maxItems", ARRAY));
      }
    maxItems_ = maxItems;
    }

  /**
   * Returns the maximum size of array values.
   */
  public Integer getMaxItems()
    {
    return maxItems_;
    }

  /**
   * Changes if array items must be unique.
   */
  public void setUniqueItems( Boolean uniqueItems)
    {
    if( uniqueItems != null)
      {
      setType( assertRequiredType( "uniqueItems", ARRAY));
      }
    uniqueItems_ = uniqueItems;
    }

  /**
   * Returns if array items must be unique.
   */
  public Boolean getUniqueItems()
    {
    return uniqueItems_;
    }

  /**
   * Changes the schema for array items.
   */
  public void setItems( Schema items)
    {
    if( items != null)
      {
      setType( assertRequiredType( "items", ARRAY));
      }
    items_ = items;
    }

  /**
   * Returns the schema for array items.
   */
  public Schema getItems()
    {
    return items_;
    }

  /**
   * Returns true if this schema is a "classifier" that describes a set values
   * that have specific properties. Returns false if this schema enumerates
   * a specific set of values.
   */
  public boolean isClassifier()
    {
    return 
      !(getMinimum() == null
        && getMaximum() == null
        && getExclusiveMinimum() == null
        && getExclusiveMaximum() == null
        && getMultipleOf() == null
        && getMinLength() == null
        && getMaxLength() == null
        && getPattern() == null
        && getMinItems() == null
        && getMaxItems() == null
        && getUniqueItems() == null
        && getItems() == null);
    }

  /**
   * Reports a failure if the given type is not defined.
   */
  private Type assertType( Type type)
    {
    if( type == null)
      {
      throw new IllegalArgumentException( "No schema type defined");
      }
    if( type == OBJECT)
      {
      throw new IllegalArgumentException( "Schema type=object is not supported");
      }
    
    return type;
    }

  /**
   * Reports a failure if this schema does not have one of the types required for the given property.
   */
  private Type assertRequiredType( String property, Type... requiredTypes)
    {
    if( Arrays.stream( requiredTypes).noneMatch( type -> type == getType()))
      {
      throw new IllegalArgumentException( String.format( "Property=%s is not allowed for schema type=%s", property, getType()));
      }

    Type thisType = getType();
    return
      thisType == NULL && requiredTypes.length > 0
      ? requiredTypes[0]
      : thisType;
    }

  /**
   * Reports a failure if this schema does not have the numeric type required for the given property value.
   */
  private Type assertRequiredNumber( String property, BigDecimal number)
    {
    assertRequiredType( property, NUMBER, INTEGER);

    Type thisType = getType();
    if( thisType == INTEGER && number.scale() != 0)
      {
      throw new IllegalArgumentException( String.format( "Property=%s must be an integer for schema type=integer", property));
      }
    
    return
      thisType == NULL
      ? NUMBER
      : thisType;
    }

  /**
   * Reports a failure if this schema does not have the type required for the given value.
   */
  private Type assertValueType( String property, DataValue<?> value)
    {
    Type type;
    if( value == null)
      {
      type = getType();
      }
    else
      {
      Type valueType = value.getType();
      Type thisType = getType();
      if( !(valueType == thisType || valueType == NULL || thisType == NULL || (thisType == NUMBER && valueType == INTEGER)))
        {
        throw new IllegalArgumentException( String.format( "'%s' value of type=%s is not allowed for schema type=%s", property, valueType, thisType));
        }

      type =
        thisType == NULL
        ? valueType
        : thisType;
      }

    return type;
    }

  /**
   * Reports a failure if this schema does not have the type required for the given value list.
   */
  private Type assertValueType( String property, Collection<DataValue<?>> values)
    {
    Map<Type,List<DataValue<?>>> valuesByType =
      Optional.ofNullable( values).orElse( emptyList())
      .stream()
      .filter( value -> value.getType() != NULL)
      .collect( groupingBy( value -> Optional.of( value.getType()).filter( type -> !type.isNumeric()).orElse( NUMBER)));

    Optional<Type> numericType =
      Optional.ofNullable( valuesByType.get( NUMBER))
      .map( numbers -> numbers.stream().map( DataValue::getType).filter( type -> type == NUMBER).findFirst().orElse( INTEGER));

    Type type;
    if( valuesByType.isEmpty())
      {
      type = getType();
      }

    else if( valuesByType.size() == 1)
      {
      DataValue<?> valueOfType =
        numericType
        .flatMap( t -> values.stream().filter( value -> value.getType() == t).findFirst())
        .orElse( values.iterator().next());

      type = assertValueType( property, valueOfType);
      }

    else
      {
      throw
        new IllegalArgumentException(
          String.format(
            "'%s' values with mixed types (%s) are not allowed",
            property,
            
            valuesByType.keySet()
            .stream()
            .map( t -> t == NUMBER? numericType.get() : t)
            .map( t -> String.valueOf( t).toLowerCase())
            .sorted()
            .collect( joining( ","))));
      }

    return type;
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getType())
      .toString();
    }

  @Override
  public boolean equals( Object object)
    {
    Schema other =
      object != null && object.getClass().equals( getClass())
      ? (Schema) object
      : null;

    return
      other != null
      && Objects.equals( other.getType(), getType())
      && Objects.equals( other.getConstant(), getConstant())
      && Objects.equals( other.getEnum(), getEnum())
      && Objects.equals( other.getFormat(), getFormat())
      && Objects.equals( other.getMinimum(), getMinimum())
      && Objects.equals( other.getMaximum(), getMaximum())
      && Objects.equals( other.getExclusiveMinimum(), getExclusiveMinimum())
      && Objects.equals( other.getExclusiveMaximum(), getExclusiveMaximum())
      && Objects.equals( other.getMultipleOf(), getMultipleOf())
      && Objects.equals( other.getMinLength(), getMinLength())
      && Objects.equals( other.getMaxLength(), getMaxLength())
      && Objects.equals( other.getPattern(), getPattern())
      && Objects.equals( other.getMinItems(), getMinItems())
      && Objects.equals( other.getMaxItems(), getMaxItems())
      && Objects.equals( other.getUniqueItems(), getUniqueItems())
      && Objects.equals( other.getItems(), getItems())
      ;
    }

  @Override
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( getType())
      ^ Objects.hashCode( getConstant())
      ^ Objects.hashCode( getEnum())
      ^ Objects.hashCode( getFormat())
      ^ Objects.hashCode( getMinimum())
      ^ Objects.hashCode( getMaximum())
      ^ Objects.hashCode( getExclusiveMinimum())
      ^ Objects.hashCode( getExclusiveMaximum())
      ^ Objects.hashCode( getMultipleOf())
      ^ Objects.hashCode( getMinLength())
      ^ Objects.hashCode( getMaxLength())
      ^ Objects.hashCode( getPattern())
      ^ Objects.hashCode( getMinItems())
      ^ Objects.hashCode( getMaxItems())
      ^ Objects.hashCode( getUniqueItems())
      ^ Objects.hashCode( getItems())
      ;
    }

  /**
   * Returns a constant hash code value defined by the properties of this schema.
   */
  public int propertyHashCode()
    {
    return
      String.valueOf( getType()).hashCode()
      ^ String.valueOf( getConstant()).hashCode()
      ^ String.valueOf( getEnum()).hashCode()
      ^ String.valueOf( getFormat()).hashCode()
      ^ String.valueOf( getMinimum()).hashCode()
      ^ String.valueOf( getMaximum()).hashCode()
      ^ String.valueOf( getExclusiveMinimum()).hashCode()
      ^ String.valueOf( getExclusiveMaximum()).hashCode()
      ^ String.valueOf( getMultipleOf()).hashCode()
      ^ String.valueOf( getMinLength()).hashCode()
      ^ String.valueOf( getMaxLength()).hashCode()
      ^ String.valueOf( getPattern()).hashCode()
      ^ String.valueOf( getMinItems()).hashCode()
      ^ String.valueOf( getMaxItems()).hashCode()
      ^ String.valueOf( getUniqueItems()).hashCode()
      ^ String.valueOf( getItems()).hashCode()
      ;
    }
  
  private Type type_;
  private DataValue<?> constant_;
  private Set<DataValue<?>> enums_;
  private String format_;
  private BigDecimal minimum_;
  private BigDecimal maximum_;
  private BigDecimal exclusiveMinimum_;
  private BigDecimal exclusiveMaximum_;
  private BigDecimal multipleOf_;
  private Integer minLength_;
  private Integer maxLength_;
  private String pattern_;
  private Integer minItems_;
  private Integer maxItems_;
  private Boolean uniqueItems_;
  private Schema items_;
  }

