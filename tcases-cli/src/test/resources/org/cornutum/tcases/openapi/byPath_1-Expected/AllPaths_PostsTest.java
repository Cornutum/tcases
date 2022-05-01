package org.cornutum.examples;


import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.junit.Test;

import java.util.Map;
import static java.util.stream.Collectors.toMap;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class AllPaths_PostsTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "0")
            .when()
                .request( "GET", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "100|93|41|58")
            .when()
                .request( "GET", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", (String) null)
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "27")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsSize_Is_5() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "0|85|16|15|7")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Size=5
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "%lB,,iZ7s")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "-1")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_101() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "101")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Contains.Value.Is=101
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void getPosts_IdsItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "ids", "0|82|16|16")
            .when()
                .request( "GET", "/posts")
            .then()
                // ids.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "GET", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "GET", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,2345")
                .header( "X-User-Id", "0")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_2345() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "2345,7700")
                .header( "X-User-Id", "305257074")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_7700() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "7700,1001")
                .header( "X-User-Id", "0")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-User-Id", "633125734")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "")
                .header( "X-User-Id", "908798277")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "true")
                .header( "X-User-Id", "45784771")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001")
                .header( "X-User-Id", "454534268")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,2345,7700")
                .header( "X-User-Id", "199793883")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", ",1001")
                .header( "X-User-Id", "530075126")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "true,2345")
                .header( "X-User-Id", "685227360")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "220811161,1001")
                .header( "X-User-Id", "807325043")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Contains.Value.Is=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XPostTypesItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,1001")
                .header( "X-User-Id", "212382841")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-Post-Types.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,2345")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,2345")
                .header( "X-User-Id", "")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,2345")
                .header( "X-User-Id", "EC]h<,G\"3+*")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void optionsPosts_XUserIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .header( "X-Post-Types", "1001,7700")
                .header( "X-User-Id", "-1")
            .when()
                .request( "OPTIONS", "/posts")
            .then()
                // X-User-Id.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "OPTIONS", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "OPTIONS", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "true")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"^@z.org\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedValue_Is_False() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_32() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "true")
                .contentType( "application/json")
                .request().body( "{\"text\":\"R)Z=aRk9 O X699I4brEZc)}_H]~YGFQ[u7+[Y{1jvMl\\\\?fP`ZaHE\\\\}tmHpOpb:$\",\"email\":\"7l`.TTd.+7p.TsH.ix;@eGpUw8WI.gov\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "POST", "/posts")
            .then()
                // approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", null)
            .when()
                .request( "POST", "/posts")
            .then()
                // approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_ApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "864.8")
            .when()
                .request( "POST", "/posts")
            .then()
                // approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/xml")
                .request().body( "[\"7&)E$\\\\1\",\"b;7\",\"lJ}rpY>H\"]")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "null")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "-394")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":null}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":{\"ojuslephkogs\":\"O+\\\"q[j\"}}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_6() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"fR.gov\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Value.Length=6
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_33() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"rROlqhu.sK`/Fa9@P.bYh.ZiC.Ppf.net\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.email.Value.Length=33
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"email\":\"'@e.gov\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":null,\"email\":\"^@j.org\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_NotString() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":{\"xtgcbcxgfoi\":true,\"rqnrarsvehz\":\"t\",\"kxodoxjnhruqlc\":-55},\"email\":\"f@y.org\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Type=Not string
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextValueLength_Is_65() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"5Tr|w`=C8;V~x]FkwIVg-us5r0!]cKW/BU<tb$Q2<fbMlh$?7iFs\\\"Nx$~*VJQWa{@\",\"email\":\"j@9.net\"}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.text.Value.Length=65
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "approved", "false")
                .contentType( "application/json")
                .request().body( "{\"text\":\"\",\"email\":\"z@6.com\",\"i\":true,\"zhbygokxf\":true,\"hbwvniykc\":true}")
            .when()
                .request( "POST", "/posts")
            .then()
                // Body.application-json.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "POST", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "POST", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "f")
                .cookie( "region", "U")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"|@g.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "3?yf*R>MA(|%pz}@")
                .cookie( "region", "x8U}z3%?N$)|r!FQ")
                .contentType( "text/plain")
                .request().body( "{\"text\":\")@Pxh_%0+3b#tkKys=2JtkX\\\"mXVZ]bCFkngr[zxur1da$%Pz\\\"a>Orl/(Y}a_q-M5\",\"email\":\"8rM.?GYI.3Xq$@8g.sOO.YAD.VG3.gov\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"'@2.edu\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", null)
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"|@4.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "true")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"O@T.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "region", ")")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"{@0.com\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.country.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", null)
                .cookie( "region", "F")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"=@M.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.country.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "j'AJKsh61`bvz&f^7")
                .cookie( "region", ".")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"r@F.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.country.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "<")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"k@b.com\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.region.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "%")
                .cookie( "region", null)
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"r@m.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.region.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_17() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "m")
                .cookie( "region", "F~v>f|K%35t[U3nn}")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"P@v.edu\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.region.Value.Length=17
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_PostIdValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "W")
                .cookie( "region", "0")
                .cookie( "xtrylgzujvaskrsm", "-+%")
                .cookie( "osgrs", "-625.2")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"=@Q.gov\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // postId.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "@")
                .cookie( "region", "o")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "q")
                .cookie( "region", "q")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "string", " (&O;=")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "t")
                .cookie( "region", ".")
                .contentType( "text/plain")
                .request().body( "")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", ")")
                .cookie( "region", "H")
                .contentType( "text/plain")
                .request().body( "-463")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "B")
                .cookie( "region", "'")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "U")
                .cookie( "region", "{")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":null}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_6() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "V")
                .cookie( "region", "q")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"F7.net\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Value.Length=6
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_33() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "Y")
                .cookie( "region", "D")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"Ku.#5c._&*.!`~.fPV@j.0c.bm.I4.edu\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.email.Value.Length=33
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "_")
                .cookie( "region", "<")
                .contentType( "text/plain")
                .request().body( "{\"email\":\"F@Y.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.text.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "J")
                .cookie( "region", "&")
                .contentType( "text/plain")
                .request().body( "{\"text\":null,\"email\":\"+@J.gov\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.text.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextValueLength_Is_65() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "#")
                .cookie( "region", "$")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"JPt TLg/#+P9F03USC(F12J)k+BE<&oGTJh<RrmNe6-yTg=G<?qA\\\\MHn+L:c\\\"B7~'\",\"email\":\"q@2.org\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.text.Value.Length=65
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "country", "O")
                .cookie( "region", "6")
                .contentType( "text/plain")
                .request().body( "{\"text\":\"\",\"email\":\"g@I.org\",\"qlrcnlkwzq\":\"[oIUO\"}")
            .when()
                .request( "PUT", "/posts")
            .then()
                // Body.text-plain.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "A")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "B|A")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_C() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "C")
            .when()
                .request( "TRACE", "/posts")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", null)
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "568.0")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "A|B|C")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "|B")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "jLId'31*^UjExCZerfw#(S_b#m[(L&~c@x{Xp7Btyr+R+'p[jrH_o$X`LnQgI'mW?MxXRGcS[=P2R2jAx5z4OLaEDo=8cde$AvZR#F#0>Z[cld8Em_%vE!{(9$+z!(Ghc.{J[m^1HdT/7PKLdR0v:@srfapHf(c/l2s&<{QFg8oMlvF<0@dbS:.5ef?WOZ+>MHIUD-{?yH$5-DM3*T>hR%ct'(7EzCR]8|A")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Contains.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePosts_PostIdItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .cookie( "postId", "A|A")
            .when()
                .request( "TRACE", "/posts")
            .then()
                // postId.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts", response.statusCode(), responseHeaders( response));
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static String forTestServer() {
        return forTestServer( null);
    }

    private static String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private static String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }

    private static Map<String,String> responseHeaders( Response response) {
        return
            response.getHeaders().asList().stream()
            .collect( toMap( Header::getName, Header::getValue));
    }
}
