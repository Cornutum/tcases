//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.util.ToString;

import java.util.Optional;

/**
 * Defines options used to generate Tcases models from OpenAPI models.
 */
public class ModelOptions
  {
  /**
   * Defines the source of API input definitions.
   */
  public enum Source
    {
      /**
       * Derive the API input model from the examples defined by the OpenAPI definition.
       */
      EXAMPLES,

      /**
       * Derive the API input model from the schemas defined by the OpenAPI definition.
       */
      SCHEMAS
    };

  /**
   * Creates a new ModelOptions instance.
   */
  public ModelOptions()
    {
    setConditionNotifier( ModelConditionNotifier.log());
    setSource( Source.SCHEMAS);
    }

  /**
   * Changes the {@link ModelConditionNotifier} that reports conditions found when creating a Tcases model from an OpenAPI model.
   */
  public void setConditionNotifier( ModelConditionNotifier notifier)
    {
    notifier_ = Optional.ofNullable( notifier).orElse( ModelConditionNotifier.ignore());
    }

  /**
   * Returns the {@link ModelConditionNotifier} that reports conditions found when creating a Tcases model from an OpenAPI model.
   */
  public ModelConditionNotifier getConditionNotifier()
    {
    return notifier_;
    }

  /**
   * Changes if the API will strictly enforce the exclusion of "readOnly" properties from requests. 
   */
  public void setReadOnlyEnforced( boolean enforced)
    {
    readOnlyEnforced_ = enforced;
    }

  /**
   * Returns if the API will strictly enforce the exclusion of "readOnly" properties from requests. 
   */
  public boolean isReadOnlyEnforced()
    {
    return readOnlyEnforced_;
    }

  /**
   * Changes if the API will strictly enforce the exclusion of "writeOnly" properties from responses. 
   */
  public void setWriteOnlyEnforced( boolean enforced)
    {
    writeOnlyEnforced_ = enforced;
    }

  /**
   * Returns if the API will strictly enforce the exclusion of "writeOnly" properties from responses. 
   */
  public boolean isWriteOnlyEnforced()
    {
    return writeOnlyEnforced_;
    }

  /**
   * Changes the source of API input definitions.
   */
  public void setSource( Source source)
    {
    source_ = source;
    }

  /**
   * Returns the source of API input definitions.
   */
  public Source getSource()
    {
    return source_;
    }

  /**
   * Returns a new ModelOptions builder.
   */
  public static Builder builder()
    {
    return builder( null);
    }

  /**
   * Returns a new ModelOptions builder.
   */
  public static Builder builder( ModelOptions defaults)
    {
    return new Builder( defaults);
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "source", getSource())
      .append( "conditionNotifier", getConditionNotifier())
      .append( "readOnlyEnforced", isReadOnlyEnforced())
      .append( "writeOnlyEnforced", isWriteOnlyEnforced())
      .toString();
    }
  
  /**
   * Builds a new {@link ModelOptions} instance.
   */
  public static class Builder
    {
    public Builder()
      {
      this( null);
      }

    public Builder( ModelOptions defaults)
      {
      modelOptions_ = new ModelOptions();
      if( defaults != null)
        {
        notifier( defaults.getConditionNotifier())
        .readOnlyEnforced( defaults.isReadOnlyEnforced())
        .writeOnlyEnforced( defaults.isWriteOnlyEnforced())
        .source( defaults.getSource());
        }
      }

    public Builder notifier( ModelConditionNotifier notifier)
      {
      modelOptions_.setConditionNotifier( notifier);
      return this;
      }

    public Builder readOnlyEnforced( boolean enforced)
      {
      modelOptions_.setReadOnlyEnforced( enforced);
      return this;
      }

    public Builder readOnlyEnforced()
      {
      return readOnlyEnforced( true);
      }

    public Builder writeOnlyEnforced( boolean enforced)
      {
      modelOptions_.setWriteOnlyEnforced( enforced);
      return this;
      }

    public Builder writeOnlyEnforced()
      {
      return writeOnlyEnforced( true);
      }

    public Builder source( Source source)
      {
      modelOptions_.setSource( source);
      return this;
      }

    public ModelOptions build()
      {
      return modelOptions_;
      }
      
    private ModelOptions modelOptions_;
    }
  
  private ModelConditionNotifier notifier_;
  private boolean readOnlyEnforced_;
  private boolean writeOnlyEnforced_;
  private Source source_;
  }
