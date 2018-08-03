//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.tcases.util.ToString;

/**
 * Defines options that control test case reducing.
 *
 */
public class ReducerOptions 
  {
  /**
   * Creates a new ReducerOptions object.
   */
  public ReducerOptions()
    {
    setSamples( 10);
    setResampleFactor( 0.0);
    }
  
  /**
   * Changes the function for test case reduction. If null, reduce test cases for all functions.
   */
  public void setFunction( String function)
    {
    function_ = function;
    }

  /**
   * Returns the function for test case reduction. If null, reduce test cases for all functions.
   */
  public String getFunction()
    {
    return function_;
    }

  /**
   * Changes the initial number of samples.
   */
  public void setSamples( int samples)
    {
    samples_ = samples;
    }

  /**
   * Returns the initial number of samples.
   */
  public int getSamples()
    {
    return samples_;
    }

  /**
   * Changes the resample factor.
   * The <CODE>resampleFactor</CODE> determines the number of samples in the next round of reducing.
   * Depending on the <CODE>resampleFactor</CODE>, the next round may use more or fewer samples.
   * <P/>
   * If the previous round called for <CODE>N</CODE> samples and produced a reduction, then the number of samples for the
   * next round will be <CODE>N * ( 1 + resampleFactor)</CODE>. To increase sample count with each round, define
   * <CODE>resampleFactor</CODE> &gt; 0.  To decrease sample count with each round, define -1 &lt;
   * <CODE>resampleFactor</CODE> &lt; 0.
   */
  public void setResampleFactor( double resampleFactor)
    {
    resampleFactor_ = resampleFactor;
    }

  /**
   * Returns the {@link #setResampleFactor resample factor}.
   */
  public double getResampleFactor()
    {
    return resampleFactor_;
    }

  /**
   * Changes if test case reduction starts by ignoring any previous random seed.
   */
  public void setNewSeed( boolean newSeed)
    {
    newSeed_ = newSeed;
    }

  /**
   * Returns if test case reduction starts by ignoring any previous random seed.
   */
  public boolean isNewSeed()
    {
    return newSeed_;
    }    

  /**
   * Returns a new ReducerOptions builder.
   */
  public static Builder builder()
    {
    return new Builder();
    }

  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( "function", getFunction())
      .append( "samples", getSamples()) 
      .append( "resampleFactor", getResampleFactor())
      .append( "newSeed", isNewSeed())
      .build();
    }

  private String function_;
  private double resampleFactor_;
  private int samples_;
  private boolean newSeed_;    

  public static class Builder
    {
    public Builder()
      {
      options_ = new ReducerOptions();
      }

    public Builder function( String function)
      {
      options_.setFunction( function);
      return this;
      }

    public Builder resampleFactor( double resampleFactor)
      {
      options_.setResampleFactor( resampleFactor);
      return this;
      }

    public Builder samples( int samples)
      {
      options_.setSamples( samples);
      return this;
      }

    public Builder newSeed( boolean newSeed)
      {
      options_.setNewSeed( newSeed);
      return this;
      } 

    public ReducerOptions build()
      {
      return options_;
      }
      
    private ReducerOptions options_;
    }
  }

