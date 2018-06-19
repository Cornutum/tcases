package org.cornutum.tcases.annotation.parser;

import org.cornutum.tcases.conditions.*;

public class ConditionReader {

  static ICondition getCondition(String[] when, String[] whenNot) {
    ICondition condition = null;

      if(when.length > 0)
        {
            condition = new ContainsAll(when);
        }

      if(whenNot.length > 0)
      {
          ICondition excludes = new Not().add(new ContainsAny(whenNot));

          condition =
                  condition == null
                          ? excludes
                          : new AllOf().add( condition).add( excludes);
      }

      return condition;
  }
}
