package org.cornutum.tcases;

import org.cornutum.tcases.validation.DefinitionsValidator;

/**
 * Global constants, can be parametrized.
 */
public abstract class Globals
  {

  private static DefinitionsValidator validator;
  private static String notApplicableName;
  private static VarValueDef notApplicableVarValue;


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

  public static void setNotApplicableName(String notApplicableName)
    {
    Globals.notApplicableName = notApplicableName;
    }

  public static String getNotApplicableName()
    {
    if (notApplicableName == null)
      {
      notApplicableName = "NA";
      }
    return notApplicableName;
    }

  /**
   * Returns true if the given value is the standard "not applicable" value.
   */
  public static boolean isNA( VarValueDef value)
    {
    return getNotApplicableName().equals(value.getName());
    }

  public static VarValueDef getNotApplicableVarValue()
    {
    if (notApplicableVarValue == null)
      {
        notApplicableVarValue = new VarValueDef(getNotApplicableName());
      }
      return notApplicableVarValue;
    }
  }
