package org.cornutum.tcases;

import org.cornutum.tcases.validation.DefinitionsValidator;

/**
 * Global constants, can be parametrized.
 */
public abstract class Globals
  {

  private static DefinitionsValidator validator;


  public static DefinitionsValidator getInputDefValidator()
    {
    if (validator == null)
      {
      validator = new DefinitionsValidator();
      }
    return validator;
    }

  public static void setValidator(DefinitionsValidator validator)
    {
    Globals.validator = validator;
    }

  }
