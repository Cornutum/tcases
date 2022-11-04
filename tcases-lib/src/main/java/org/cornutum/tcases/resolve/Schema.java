//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.resolve.DataValue.Type;

import java.math.BigDecimal;
import java.util.List;

/**
 * Defines requirements for the value of a system input variable.
 */
public class Schema
  {
  /**
   * Creates a new Schema instance.
   */
  public Schema()
    {
    }
  
  /**
   * Changes the type of input values.
   */
  public void setType( Type type)
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
   * Changes the enumeration of input values.
   */
  public void setEnums( List<DataValue<?>> enums)
    {
    enums_ = enums;
    }

  /**
   * Returns the enumeration of input values.
   */
  public List<DataValue<?>> getEnums()
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
    items_ = items;
    }

  /**
   * Returns the schema for array items.
   */
  public Schema getItems()
    {
    return items_;
    }

  private Type type_;
  private List<DataValue<?>> enums_;
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

