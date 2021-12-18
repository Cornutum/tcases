//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import java.util.Iterator;
import java.util.Optional;

/**
 * Defines a list of values.
 */
public class ListVar extends VarSet
  {
  /**
   * Creates a new ListVar instance.
   */
  public ListVar()
    {
    this( null);
    }
  
  /**
   * Creates a new ListVar instance.
   */
  public ListVar( String name)
    {
    super( name);
    setSize( null, null);
    }

  /**
   * Returns true if this variable defines a list of values.
   */
  @Override
  public boolean isList()
    {
    return true;
    }

  /**
   * Adds an input variable to this set.
   */
  @Override
  public VarSet addMember( IVarDef var)
    {
    throw new UnsupportedOperationException( String.format( "Can't change the members that represent a %s", getClass().getSimpleName()));
    }

  /**
   * Removes an input variable from this set.
   */
  @Override
  public VarSet removeMember( String name)
    {
    throw new UnsupportedOperationException( String.format( "Can't change the members that represent a %s", getClass().getSimpleName()));
    }

  /**
   * Changes the size of this list.
   */
  public void setSize( Integer minSize, Integer maxSize)
    {
    if( !(minSize == null || minSize >= 0))
      {
      throw new IllegalArgumentException( String.format( "Invalid minSize=%s -- must be non-negative", minSize));
      }
    minSize_ = minSize;
    
    if( !(maxSize == null || maxSize >= 0))
      {
      throw new IllegalArgumentException( String.format( "Invalid maxSize=%s -- must be non-negative", maxSize));
      }
    maxSize_ = maxSize;

    if( Optional.ofNullable( maxSize).filter( max -> max < Optional.ofNullable( minSize).orElse( 0)).isPresent())
      {
      throw new IllegalArgumentException( String.format( "max=%s is less than min=%s", maxSize, minSize));
      }

    // (Re)define the size input variable model for this list.
    VarDef sizeVar = (VarDef) getMember( "Size");
    if( sizeVar == null)
      {
      sizeVar = new VarDef( "Size");
      super.addMember( sizeVar);
      }
    else
      {
      for( Iterator<VarValueDef> values = sizeVar.getValues();
           values.hasNext();
           values.next(), values.remove());
      }
      
    VarDefBuilder sizeBuilder = VarDefBuilder.with( sizeVar);

    Optional<Integer> minValue = Optional.ofNullable( getMinSize());
    minValue.filter( min -> min > 0).ifPresent( min -> {
      sizeBuilder.values( VarValueDefBuilder.with( min - 1).type( VarValueDef.Type.FAILURE).build());
      });
    sizeBuilder.values( VarValueDefBuilder.with( minValue.orElse( 0)).build());

    Optional<Integer> maxValue = Optional.ofNullable( getMaxSize());;
    maxValue.ifPresent( max -> {
      sizeBuilder.values(
        VarValueDefBuilder.with( max).build(),
        VarValueDefBuilder.with( max + 1).type( VarValueDef.Type.FAILURE).build());
      });
    if( !maxValue.isPresent())
      {
      sizeBuilder.values( VarValueDefBuilder.with( String.format( "> %s", minValue.orElse( 0))).build());
      }
    }

  /**
   * Returns the minimum size of this list.
   */
  public Integer getMinSize()
    {
    return minSize_;
    }

  /**
   * Returns the maximum size of this list.
   */
  public Integer getMaxSize()
    {
    return maxSize_;
    }

  /**
   * Changes the name of the property used to count list members.
   */
  public void setMemberCountProperty( String property)
    {
    memberCountProperty_ = property;
    }

  /**
   * Returns the name of the property used to count list members.
   */
  public String getMemberCountProperty()
    {
    return memberCountProperty_;
    }

  /**
   * Changes the input definition for list members.
   */
  public void setMemberVarDef( AbstractVarDef memberVarDef)
    {
    memberVarDef.setName( "Member");
    memberVarDef.setCondition( null);
    super.addMember( memberVarDef);
    }

  /**
   * Returns the input definition for list members.
   */
  public IVarDef getMemberVarDef()
    {
    return getMember( "Member");
    }

  private Integer minSize_ = 0;
  private Integer maxSize_ = null;
  private String  memberCountProperty_;
  }
