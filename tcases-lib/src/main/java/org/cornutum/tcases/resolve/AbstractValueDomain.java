//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

/**
 * Base class for {@link ValueDomain} implementations
 */
public abstract class AbstractValueDomain<T> implements ValueDomain<T>
  {
  /**
   * Returns a {@link DataValue} for the given value in this domain.
   */
  protected abstract DataValue<T> dataValueOf( T value);

  /**
   * Changes the format for values that belong to this domain.
   */
  public void setFormat( String format)
    {
    format_ = format;
    }

  /**
   * Returns the format for values that belong to this domain.
   */
  @Override
  public String getFormat()
    {
    return format_;
    }

  /**
   * Applies the given format to the specified {@link ValueDomain}.
   */
  public static <T,D extends AbstractValueDomain<T>> D withFormat( D domain, String format)
    {
    domain.setFormat( format);
    return domain;
    }

  private String format_;
  }
