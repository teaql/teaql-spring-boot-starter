package io.teaql.data.web;

import java.util.ArrayList;
import java.util.List;

import io.teaql.data.BaseEntity;
import io.teaql.data.SmartList;

public class WebResponse {
    List<BaseEntity> data;
    private int resultCode;
    private String status;
    private String message;
    private int recordCount;

    public WebResponse() {
        data = new ArrayList<>();
    }

    public static WebResponse of(List<? extends BaseEntity> list) {
        WebResponse webResponse = success();
        if (list == null || list.isEmpty()) {
            return webResponse;
        }
        webResponse.setRecordCount(list.size());
        webResponse.getData().addAll(list);
        return webResponse;
    }

    public static WebResponse emptyList(String message) {
        WebResponse webResponse = new WebResponse();
        webResponse.setResultCode(0);
        webResponse.setMessage(message);
        return webResponse;
    }

    public static WebResponse success() {
        WebResponse webResponse = new WebResponse();
        webResponse.setResultCode(0);
        webResponse.setStatus("YES");
        return webResponse;
    }

    public static WebResponse fail(String message) {
        WebResponse webResponse = new WebResponse();
        webResponse.setStatus("NO");
        webResponse.setResultCode(1);
        webResponse.setMessage(message);
        return webResponse;
    }

    public static WebResponse of(SmartList<? extends BaseEntity> smartList) {
        WebResponse webResponse = success();
        if (smartList == null || smartList.isEmpty()) {
            return webResponse;
        }
        webResponse.setRecordCount(smartList.getTotalCount());
        webResponse.getData().addAll(smartList.getData());
        return webResponse;
    }

    public static WebResponse of(BaseEntity entity) {
        WebResponse webResponse = success();
        if (entity != null) {
            webResponse.data.add(entity);
            webResponse.setRecordCount(1);
        }
        return webResponse;
    }

    public int getRecordCount() {
        return recordCount;
    }

    public void setRecordCount(int recordCount) {
        this.recordCount = recordCount;
    }

    public List<BaseEntity> getData() {
        if (data == null) {
            data = new ArrayList<>();
        }
        return data;
    }

    public void setData(List<BaseEntity> data) {
        this.data = data;
    }

    public int getResultCode() {
        return resultCode;
    }

    public void setResultCode(int resultCode) {
        this.resultCode = resultCode;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
