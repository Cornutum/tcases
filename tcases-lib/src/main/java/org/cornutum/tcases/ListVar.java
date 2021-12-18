//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2021, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

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
    for( Iterator<VarValueDef> values = sizeVarDef_.getValues();
         values.hasNext();
         values.next(), values.remove());
      
    VarDefBuilder sizeBuilder = VarDefBuilder.with( sizeVarDef_);

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
    memberVarDef_ = memberVarDef;
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
    return sizeVarDef_;
    }

  /**
   * Returns a list of member variables.
   */
  @Override
  protected List<IVarDef> getMemberVarDefs()
    {
    return
      Stream.of( getSizeVarDef(), getMemberVarDef())
      .filter( Objects::nonNull)
      .collect( toList());
    }

  private Integer minSize_ = 0;
  private Integer maxSize_ = null;
  private String  memberCountProperty_;
  private VarDef  sizeVarDef_ = new VarDef( "Size");
  private IVarDef memberVarDef_;
  }
