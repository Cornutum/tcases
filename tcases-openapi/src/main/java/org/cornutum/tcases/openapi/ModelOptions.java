//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import java.util.Optional;

/**
 * Defines options used to generate Tcases models from OpenAPI models.
 */
public class ModelOptions
  {
  /**
   * Creates a new ModelOptions instance.
   */
  public ModelOptions()
    {
    setConditionNotifier( ModelConditionNotifier.log());
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
   * Returns a new ModelOptions builder.
   */
  public static Builder builder()
    {
    return new Builder();
    }

  /**
   * Builds a new {@link ModelOptions} instance.
   */
  public static class Builder
    {
    public Builder()
      {
      modelOptions_ = new ModelOptions();
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

    public ModelOptions build()
      {
      return modelOptions_;
      }
      
    private ModelOptions modelOptions_;
    }
  
  private ModelConditionNotifier notifier_;
  private boolean readOnlyEnforced_;
  private boolean writeOnlyEnforced_;
  }
