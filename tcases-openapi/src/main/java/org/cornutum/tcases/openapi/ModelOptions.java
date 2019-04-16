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
   * Returns a new ModelOptions builder.
   */
  public static Builder builder()
    {
    return new Builder();
    }

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

    public ModelOptions build()
      {
      return modelOptions_;
      }
      
    private ModelOptions modelOptions_;
    }
  
  private ModelConditionNotifier notifier_;
  }
