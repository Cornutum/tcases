//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.conditions.AllOf;
import org.cornutum.tcases.conditions.ICondition;
import org.cornutum.tcases.util.ToString;

import org.apache.commons.lang3.ArrayUtils;

import java.util.Iterator;
import java.util.concurrent.atomic.AtomicInteger;

import static org.cornutum.tcases.Globals.getInputDefValidator;

/**
 * Base class for {@link IVarDef} implementations.
 *
 */
public abstract class AbstractVarDef extends Conditional implements IVarDef
  {
  /**
   * Defines the position of a variable definition within a function input definition.
   *
   */
  private static class Position implements IVarDef.Position
    {    
    /**
     * Creates a new Position object.
     */
    public Position( IVarDef parent, int seqNum)
      {
      int[] parentPosition =
        parent == null
        ? null
        : toPosition( parent.getPosition()).path_;
      
      path_ = ArrayUtils.add( parentPosition, seqNum);
      }

    public int compareTo( IVarDef.Position other)
      {
      Position position = toPosition( other);

      // Position with smaller ancestor is smaller.
      int delta = 0;
      int parentLength = Math.min( path_.length, position.path_.length);
      for( int i = 0; i < parentLength && (delta = path_[i] - position.path_[i]) == 0; i++);

      if( delta == 0)
        {
        // Ancestor is smaller than descendant.
        delta = path_.length - position.path_.length;
        }
      
      return delta;
      }

    private Position toPosition( IVarDef.Position other)
      {
       Position position =
        other != null && other.getClass().equals( getClass())
        ? (Position) other
        : null;

      if( position == null)
        {
        throw new IllegalArgumentException( String.valueOf( this) + " is not comparable with " + other);
        }

      return position;
      }
    
    private int[] path_;
    }


  /**
   * Creates a new AbstractVarDef object.
   */
  public AbstractVarDef()
    {
    this( null);
    }
  
  /**
   * Creates a new AbstractVarDef object.
   */
  public AbstractVarDef( String name)
    {
    setName( name);
    setType( IVarDef.ARG);
    setCondition( ICondition.ALWAYS);
    setSeqNum( getNextSeqNum());
    }
  
  /**
   * Changes the variable name.
   */
  public void setName( String name)
    {
    getInputDefValidator().assertVarDefName( name);
    name_ = name;
    pathName_ = null;
    }
  
  /**
   * Returns the variable name.
   */
  public String getName()
    {
    return name_;
    }

  /**
   * Returns the hierarchical path name of this variable.
   */
  public String getPathName()
    {
    if( pathName_ == null)
      {
      StringBuilder pathName = new StringBuilder();

      IVarDef parent = getParent();
      if( parent != null)
        {
        pathName
          .append( parent.getPathName())
          .append( '.');
        }

      String name = getName();
      if( name != null)
        {
        pathName.append( name);
        }

      pathName_ = pathName.toString();
      }

    return pathName_;
    }

  /**
   * Changes the type identifier for this variable.
   */
  public void setType( String type)
    {
    getInputDefValidator().assertVarDefType( type);
    type_ = type;
    }

  /**
   * Returns the type identifier for this variable.
   */
  public String getType()
    {
    IVarDef parent = getParent();
    return parent==null? type_ : parent.getType();
    }

  /**
   * Changes the parent of this variable.
   */
  public void setParent( IVarDef parent)
    {
    parent_ = parent;
    pathName_ = null;
    effCondition_ = null;
    position_ = null;
    }

  /**
   * If this is member of another variable, returns the parent variable. Otherwise, returns null.
   */
  public IVarDef getParent()
    {
    return parent_;
    }

  /**
   * Returns the effective condition that defines when this variable is applicable,
   * based on the conditions for this variable and all of its ancestors.
   */
  public ICondition getEffectiveCondition()
    {
    if( effCondition_ == null)
      {
      ICondition  condition           = getCondition();
      IVarDef     parent              = getParent();
      ICondition  parentEffCondition  = parent==null? null : parent.getEffectiveCondition();

      if( condition == null && parentEffCondition != null)
        {
        // Same effective condition as parent.
        effCondition_ = parentEffCondition;
        }

      else if( condition != null && parentEffCondition == null)
        {
        // No ancestor conditions to add.
        effCondition_ = condition;
        }

      else
        {
        // Create new conjunction with ancestor conditions.
        AllOf effCondition = new AllOf();
        if( condition != null)
          {
          if( parentEffCondition instanceof AllOf)
            {
            for( Iterator<ICondition> parentEffConditions = ((AllOf) parentEffCondition).getConditions();
                 parentEffConditions.hasNext();
                 effCondition.add( parentEffConditions.next()));
            }
          else
            {
            effCondition.add( parentEffCondition);
            }
          
          effCondition.add( condition);
          }

        effCondition_ = effCondition;
        }
      }
    
    return effCondition_;
    }
  
  /**
   * Changes the condition that defines when this element is applicable.
   */
  public void setCondition( ICondition condition)
    {
    super.setCondition( condition);
    effCondition_ = null;
    }

  /**
   * Changes the position of this variable definition.
   */
  private void setPosition( Position position)
    {
    position_ = position;
    }

  /**
   * Returns the position of this variable definition.
   */
  public IVarDef.Position getPosition()
    {
    if( position_ == null)
      {
      setPosition( new Position( getParent(), seqNum_));
      }
    
    return position_;
    }

  /**
   * Changes the sequence number of this variable.
   */
  public void setSeqNum( int seqNum)
    {
    seqNum_ = seqNum;
    }

  /**
   * Returns the sequence number of this variable.
   */
  public int getSeqNum()
    {
    return seqNum_;
    }

  /**
   * Returns the next variable sequence number.
   */
  protected int getNextSeqNum()
    {
    return nextSeqNum_.getAndIncrement();
    }

  /**
   * If this variable has member variables, returns an iterator for the member variable list.
   * Otherwise, returns null.
   */
  abstract public Iterator<IVarDef> getMembers();

  /**
   * If this variable defines a value set, returns an iterator for the value set.
   * Otherwise, returns null.
   */
  abstract public Iterator<VarValueDef> getValues();

  /**
   * Returns the descendant variable with the given name path, relative to this variable.
   */
  abstract public IVarDef find( String[] path);

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getPathName())
      .toString();
    }
  
  private String name_;
  private String type_;
  private IVarDef parent_;
  private String pathName_;
  private Position position_;
  private int seqNum_;
  private ICondition effCondition_;

  private static AtomicInteger nextSeqNum_ = new AtomicInteger();
  }
