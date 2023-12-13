package io.teaql.data.sql;

import io.teaql.data.RepositoryException;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

public class ResultSetTool {
    //ResultSetTool.getValue(rs,columnName)

    public static Object getValue(ResultSet resultSet, String columnName)  {
        try{
            return getValueInternal(resultSet,columnName);
        }catch (Exception e){
            throw new RepositoryException(e);
        }

    }
    protected static Object getValueInternal(ResultSet resultSet, String columnName) throws SQLException {
        ResultSetMetaData metaData = resultSet.getMetaData();

        for(int i=0;i<metaData.getColumnCount();i++){
            String columnNameInRs=metaData.getColumnName(i);
            if(columnNameInRs.equalsIgnoreCase(columnNameInRs)){
                return resultSet.getObject(i+1);
            }
        }

        throw new IllegalArgumentException("Column '"+columnName+"' is not found in ResultSet");

    }

}
