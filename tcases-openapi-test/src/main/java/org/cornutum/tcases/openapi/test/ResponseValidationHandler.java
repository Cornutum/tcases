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
   * Handles when response data does not satisfy OpenAPI requirements.
   */
  public void handleInvalid( ResponseValidationException condition);

  /**
   * Handles when response data cannot be validated.
   */
  public void handleUnvalidated( ResponseUnvalidatedException condition);

  /**
   * Reports a failure only when response data does not satisfy OpenAPI requirements.
   * All other conditions are ignored.
   */
  public ResponseValidationHandler EXPECT_CONFORM =
    new ResponseValidationHandler()
      {
      @Override
      public void handleInvalid( ResponseValidationException condition)
        {
        throw condition;
        }
      
      @Override
      public void handleUnvalidated( ResponseUnvalidatedException condition)
        {
        // Ignore this condition.
        }

      @Override
      public String toString()
        {
        return "EXPECT_CONFORM";
        }
      };

  /**
   * Ignores all validation conditions.
   */
  public ResponseValidationHandler IGNORE_ALL =
    new ResponseValidationHandler()
      {
      @Override
      public void handleInvalid( ResponseValidationException condition)
        {
        // Ignore this condition.
        }
      
      @Override
      public void handleUnvalidated( ResponseUnvalidatedException condition)
        {
        // Ignore this condition.
        }

      @Override
      public String toString()
        {
        return "IGNORE_ALL";
        }
      };

  /**
   * Reports a failure for all validation conditions.
   */
  public ResponseValidationHandler FAIL_ALL =
    new ResponseValidationHandler()
      {
      @Override
      public void handleInvalid( ResponseValidationException condition)
        {
        throw condition;
        }
      
      @Override
      public void handleUnvalidated( ResponseUnvalidatedException condition)
        {
        throw condition;
        }

      @Override
      public String toString()
        {
        return "FAIL_ALL";
        }
      };
  }
