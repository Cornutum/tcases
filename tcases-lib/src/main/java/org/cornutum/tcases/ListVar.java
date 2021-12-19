//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import static org.cornutum.tcases.conditions.Conditions.has;

import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Defines a list of values.
 */
public class ListVar extends CompositeVar
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
    sizeVarDef_ = addComponent( new VarDef( "Size"));
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
   * Changes the size of this list.
   */
  public void setSize( Integer minSize, Integer maxSize)
    {
    if( !(minSize == null || minSize >= 0))
      {
      throw new IllegalArgumentException( String.format( "Invalid min=%s -- must be non-negative", minSize));
      }
    if( !(maxSize == null || maxSize >= 0))
      {
      throw new IllegalArgumentException( String.format( "Invalid max=%s -- must be non-negative", maxSize));
      }

    if( Optional.ofNullable( maxSize).filter( max -> max < Optional.ofNullable( minSize).orElse( 0)).isPresent())
      {
      throw new IllegalArgumentException( String.format( "max=%s is less than min=%s", maxSize, minSize));
      }

    minSize_ = minSize;
    maxSize_ = maxSize;

    clearSizeVarDef();
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
   * Changes the property used to count list members.
   */
  public void setMemberCountProperty( String property)
    {
    memberCountProperty_ = property;
    }

  /**
   * Returns the property used to count list members.
   */
  public String getMemberCountProperty()
    {
    if( memberCountProperty_ == null)
      {
      memberCountProperty_ = String.format( "members_%s", sizeVarDef_.getSeqNum());
      }
    return memberCountProperty_;
    }

  /**
   * Returns the property that indicates if a list of the given size is non-empty.
   * Returns <CODE>empty()</CODE> if <CODE>size</CODE> is zero.
   */
  private Optional<String> hasMembers( int size)
    {
    return Optional.of( getNonEmptyProperty()).filter( p -> size > 0);
    }

  /**
   * Returns the property that indicates that this list that is non-empty.
   */
  private String getNonEmptyProperty()
    {
    return String.format( "hasMembers_%s", sizeVarDef_.getSeqNum());
    }

  /**
   * Changes the input definition for list members.
   */
  public void setMemberVarDef( AbstractVarDef memberVarDef)
    {
    memberVarDef.setName( "Member");
    AbstractVarDef.class.cast( memberVarDef).setCondition( has( getNonEmptyProperty()));
    memberVarDef_ = addComponent( memberVarDef);
    }

  /**
   * Returns the input definition for list members.
   */
  public IVarDef getMemberVarDef()
    {
    return memberVarDef_;
    }

  /**
   * Returns the input definition for the size of this list.
   */
  private VarDef getSizeVarDef()
    {
    if( !sizeVarDef_.getValues().hasNext())
      {
      resetSizeVarDef();
      }
    return sizeVarDef_;
    }

  /**
   * Clears the input definition for the size of this list.
   */
  private void clearSizeVarDef()
    {
    for( Iterator<VarValueDef> values = sizeVarDef_.getValues();
         values.hasNext();
         values.next(), values.remove());
    }

  /**
   * Resets the input definition for the size of this list.
   */
  private void resetSizeVarDef()
    {
    VarDefBuilder sizeBuilder = VarDefBuilder.with( sizeVarDef_);

    int minValue = Optional.ofNullable( getMinSize()).orElse( 0);
    if( minValue > 0)
      {
      sizeBuilder.values( VarValueDefBuilder.with( minValue - 1).type( VarValueDef.Type.FAILURE).build());
      }
    sizeBuilder.values( VarValueDefBuilder.with( minValue).properties( hasMembers( minValue)).build());
    
    Optional<Integer> maxValue = Optional.ofNullable( getMaxSize());
    maxValue.ifPresent( max -> {
      sizeBuilder.values(
        VarValueDefBuilder.with( max).properties( hasMembers( max)).build(),
        VarValueDefBuilder.with( max + 1).type( VarValueDef.Type.FAILURE).build());
      });
    if( !maxValue.isPresent())
      {
      int min = minValue + 1;
      sizeBuilder.values( VarValueDefBuilder.with( String.format( ">= %s", min)).properties( hasMembers( min)).has( "minimum", min).build());
      }
    }

  /**
   * Returns a list of component variables.
   */
  @Override
  protected List<IVarDef> getComponents()
    {
    return
      Stream.of( getSizeVarDef(), getMemberVarDef())
      .filter( Objects::nonNull)
      .collect( toList());
    }

  private Integer minSize_;
  private Integer maxSize_;
  private String  memberCountProperty_;
  private VarDef  sizeVarDef_;
  private IVarDef memberVarDef_;
  }
