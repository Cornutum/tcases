package org.cornutum.examples.tests;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MyTests {

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesDefined_Is_Yes() {
        given()
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesItemsContainsValue_Is_2345() {
        given()
            .header( "X-Post-Types", "2345,7700")
            .header( "X-User-Id", "761295545")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesItemsContainsValue_Is_7700() {
        given()
            .header( "X-Post-Types", "7700,2345")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesDefined_Is_No() {
        given()
            .header( "X-User-Id", "433704959")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesType_Is_Null() {
        given()
            .header( "X-Post-Types", "")
            .header( "X-User-Id", "791204254")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesType_Is_NotArray() {
        given()
            .header( "X-Post-Types", "647")
            .header( "X-User-Id", "249283126")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesItemsSize_Is_1() {
        given()
            .header( "X-Post-Types", "1001")
            .header( "X-User-Id", "796819282")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesItemsSize_Is_3() {
        given()
            .header( "X-Post-Types", "1001,7700,2345")
            .header( "X-User-Id", "837031824")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesItemsContainsType_Is_Null() {
        given()
            .header( "X-Post-Types", ",1001")
            .header( "X-User-Id", "1012855669")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesItemsContainsType_Is_NotInteger() {
        given()
            .header( "X-Post-Types", "PyCeS/;&,XTzA Y,~H|,1001")
            .header( "X-User-Id", "944060091")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesItemsContainsValue_Is_Other() {
        given()
            .header( "X-Post-Types", "-698622611,1001")
            .header( "X-User-Id", "633311474")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Value.Is=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XPostTypesItemsUnique_Is_No() {
        given()
            .header( "X-Post-Types", "1001,1001")
            .header( "X-User-Id", "57695003")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XUserIdDefined_Is_No() {
        given()
            .header( "X-Post-Types", "1001,2345")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XUserIdType_Is_Null() {
        given()
            .header( "X-Post-Types", "1001,7700")
            .header( "X-User-Id", "")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XUserIdType_Is_NotInteger() {
        given()
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "=6^")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test(timeout=12345)
    public void optionsPosts_XUserIdValue_Is_M1() {
        given()
            .header( "X-Post-Types", "1001,7700")
            .header( "X-User-Id", "-1")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    private Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }
}
