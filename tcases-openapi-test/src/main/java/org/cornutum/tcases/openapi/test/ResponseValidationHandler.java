//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

/**
 * Handles response validation conditions.
 */
public interface ResponseValidationHandler
  {
  /**
   * Handles a condition that results in no validation of response data.
   */
  public void handleUnvalidated( ResponseUnvalidatedException condition);

  public ResponseValidationHandler IGNORE =
    new ResponseValidationHandler()
      {
      @Override
      public void handleUnvalidated( ResponseUnvalidatedException condition)
        {
        // Ignore this condition.
        }
      };
  
  public ResponseValidationHandler FAIL =
    new ResponseValidationHandler()
      {
      @Override
      public void handleUnvalidated( ResponseUnvalidatedException condition)
        {
        throw condition;
        }
      };
  }
