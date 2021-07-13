//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.conditions;

import org.cornutum.tcases.util.ToString;

/**
 * Base class for {@link IAssertion assertions} of a limit on the number of instances of a property
 *
 */
public abstract class BoundedAssertion extends AbstractAssertion
  {
  /**
   * Creates a new BoundedAssertion object.
   */
  public BoundedAssertion()
    {
    this( null, 0);
    }
  
  /**
   * Creates a new BoundedAssertion object.
   */
  public BoundedAssertion( String property, int bound)
    {
    super( property);
    setBound( bound);
    }

  /**
   * Changes the bound on the number of instances of this property.
   */
  public void setBound( int bound)
    {
    if( bound < 0)
      {
      throw new IllegalArgumentException( "Bound can't be less than 0");
      }
    bound_ = bound;
    }

  /**
   * Returns the bound on the number of instances of this property.
   */
  public int getBound()
    {
    return bound_;
    }

  /**
   * Returns true is the bound is exclusive.
   */
  public abstract boolean isExclusive();

  @Override
public int hashCode()
    {
    return
      super.hashCode()
      ^ getBound();
    }

  @Override
public boolean equals( Object object)
    {
    BoundedAssertion other =
      super.equals( object)
      ? (BoundedAssertion) object
      : null;

    return
      other != null
      && other.getBound() == getBound();
    }
  
  @Override
public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "property", getProperty())
      .append( "bound", getBound())
      .toString();
    }

  private int bound_;
  }

